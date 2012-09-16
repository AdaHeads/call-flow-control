-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                AMI.Event                                  --
--                                                                           --
--                                  BODY                                     --
--                                                                           --
--                     Copyright (C) 2012-, AdaHeads K/S                     --
--                                                                           --
--  This is free software;  you can redistribute it and/or modify it         --
--  under terms of the  GNU General Public License  as published by the      --
--  Free Software  Foundation;  either version 3,  or (at your  option) any  --
--  later version. This library is distributed in the hope that it will be   --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     --
--  You should have received a copy of the GNU General Public License and    --
--  a copy of the GCC Runtime Library Exception along with this program;     --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
--  <http://www.gnu.org/licenses/>.                                          --
--                                                                           --
-------------------------------------------------------------------------------

with Ada.Calendar,
     Ada.Exceptions,
     Ada.Strings.Unbounded;

with --  AMI.Action,
     AMI.IO,
     AMI.Protocol;

with Call_List,
     Peers;

with Yolk.Log;

package body AMI.Event is
   --  use Ada.Strings.Unbounded;
   use Call_List;
   use Ada.Exceptions;
   use AMI.IO;
   use Peers;

   function TS
     (US : in Ada.Strings.Unbounded.Unbounded_String)
      return String
      renames Ada.Strings.Unbounded.To_String;

   function TUS
     (S : in String)
      return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;

   Asterisk : Asterisk_AMI_Type;

   Event_Callback_Routine : constant Event_Callback_Routine_Table :=
     (Dial                 => Dial_Callback'Access,
      Hangup               => Hangup_Callback'Access,
      Join                 => Join_Callback'Access,
      Newchannel           => Newchannel_Callback'Access,
      PeerStatus           => PeerStatus_Callback'Access,
      Unlink               => Unlink_Callback'Access,
      others               => null);

   --  Lists agents
   --     procedure Agents is
   --     begin
   --        Put_Line ("Not implemented");
   --        raise NOT_IMPLEMENTED;
   --     end Agents;

   --  Event: Bridge
   --  Privilege: call,all
   --  Bridgestate: Link
   --  Bridgetype: core
   --  Channel1: SIP/softphone2-0000000a
   --  Channel2: SIP/softphone1-0000000b
   --  Uniqueid1: 1340097427.10
   --  Uniqueid2: 1340097427.11
   --  CallerID1: softphone2
   --  CallerID2: 100
   procedure Bridge_Callback is
   begin
      Yolk.Log.Trace (Yolk.Log.Debug, "Bridge not implemented");
      raise NOT_IMPLEMENTED;
   end Bridge_Callback;

   --  Event: Dial
   --  Privilege: call,all
   --  SubEvent: Begin
   --  Channel: SIP/softphone2-0000000a
   --  Destination: SIP/softphone1-0000000b
   --  CallerIDNum: softphone2
   --  CallerIDName: <unknown>
   --  UniqueID: 1340097427.10
   --  DestUniqueID: 1340097427.11
   --  Dialstring: softphone1
   procedure Dial_Callback (Event_List : in Event_List_Type.Map) is
      use Ada.Strings.Unbounded;
   begin
      if Event_List.Contains (TUS ("Message")) and then
        Event_List.Element  (TUS ("Message")) =
        "Authentication accepted" then
         Asterisk.Logged_In := True;
      end if;
   end Dial_Callback;

   --  Event: Hangup
   --  Privilege: call,all
   --  Channel: SIP/softphone1-0000000b
   --  Uniqueid: 1340097427.11
   --  CallerIDNum: 100
   --  CallerIDName: <unknown>
   --  Cause: 16
   --  Cause-txt: Normal Clearing
   procedure Hangup_Callback (Event_List : in Event_List_Type.Map) is
      Call : Call_Type;
      Uniqueid : Unbounded_String;
   begin
      if Event_List.Contains (TUS ("Uniqueid")) then
         Uniqueid := Event_List.Element (TUS ("Uniqueid"));
         Call := Call_List.Remove (Uniqueid);

         Yolk.Log.Trace (Yolk.Log.Debug, "This call have been hangup: " &
                           "Channel: " & TS (Call.Channel) &
                           "UniqueID: " & TS (Call.Uniqueid));
         if Call = null_Call then
            Yolk.Log.Trace
              (Yolk.Log.info,
               "Got a hangup on a call, that was not in the Calls list." &
                 " Uniqueid: " & TS (Uniqueid))
         end if;
      end if;
   end Hangup_Callback;

   --  Event: Join
   --  Privilege: call,all
   --  Channel: SIP/TP-Softphone-00000036
   --  CallerIDNum: TP-Softphone
   --  CallerIDName: unknown
   --  Queue: org_id1
   --  Position: 1
   --  Count: 1
   --  Uniqueid: 1347875403.105
   procedure Join_Callback (Event_List : in Event_List_Type.Map) is
      Call : Call_List.Call_Type := null_Call;
      --  Unknownen
   begin
      if Event_List.Contains (TUS ("Uniqueid")) then
         --  The call should exsists at this point.
         Call := Call_List.Get_Call
           (Event_List.Element (TUS ("Uniqueid")));
      end if;

      --  There are no call with that ID, something is wrong.
      if Call = null_Call then
         if Event_List.Contains (TUS ("Channel")) then
            Yolk.Log.Trace (Yolk.Log.Error,
                            "Got a Join evnet, "&
                              "on a call there is not in the call list. " &
                              "Channel: " &
                              TS (Event_List.Element
                (TUS ("Channel"))));
         else
            Yolk.Log.Trace (Yolk.Log.Error,
                            "Got a Join Event, on a call that don't exsist" &
                              " and do not have a Channel");
         end if;
      end if;

      if Call.State = Call_List.Unknown then
         Call.State := Call_List.Queued;

         if Event_List.Contains (TUS ("Queue")) then
            Call.Queue := Event_List.Element
              (TUS ("Queue"));
         end if;
      elsif Call.State = Call_List.OnHold then
         null;
      else
         Yolk.Log.Trace (Yolk.Log.Error, "Join Event, Call with bad state: " &
                           Call.State'Img);
      end if;
   end Join_Callback;

   procedure Login (Asterisk_AMI : in Asterisk_AMI_Type;
                    Username     : in String;
                    Secret       : in String) is
         Command : constant String := AMI.Protocol.Login (Username, Secret,
                                                          Async =>  False);
   begin
      AMI.IO.Send (Asterisk_AMI.Channel, Command);
   end Login;

   procedure Login_Callback (Event_List : in Event_List_Type.Map) is
      use Ada.Strings.Unbounded;
      Message : Unbounded_String;
   begin

      if Event_List.Contains (TUS ("Message")) then
         Message := Event_List.Element
           (TUS ("Message"));

         if TS (Message) =  "Authentication accepted" then
            Asterisk.Logged_In := True;
         else
            Asterisk.Logged_In := False;
         end if;
      end if;

   end Login_Callback;

   --  Event: Newchannel
   --  Privilege: call,all
   --  Channel: SIP/TP-Softphone-00000036
   --  ChannelState: 0
   --  ChannelStateDesc: Down
   --  CallerIDNum: TP-Softphone
   --  CallerIDName:
   --  AccountCode:
   --  Exten: 7001
   --  Context: LocalSets
   --  Uniqueid: 1347875403.105
   procedure Newchannel_Callback (Event_List : in Event_List_Type.Map) is
      Call : Call_Type;
   begin
      if Event_List.Contains (TUS ("Channel")) then
         Call.Channel := Event_List.Element
            (TUS ("Channel"));
      end if;

      if Event_List.Contains (TUS ("Uniqueid")) then
         Call.Uniqueid :=  Event_List.Element
           (TUS ("Uniqueid"));
      end if;

      if Event_List.Contains (TUS ("Exten")) then
         Call.Extension :=  Event_List.Element
           (TUS ("Exten"));
      end if;

      --  Save the time when the call came in.
      Call.Arrived := Ada.Calendar.Clock;

      Call_List.Add (Call);
   end Newchannel_Callback;

   --  Event: Newstate
   --  Privilege: call,all
   --  Channel: SIP/softphone1-0000000b
   --  ChannelState: 5
   --  ChannelStateDesc: Ringing
   --  CallerIDNum: 100
   --  CallerIDName:
   --  Uniqueid: 1340097427.11
   procedure NewState_Callback is
   begin
      Yolk.Log.Trace (Yolk.Log.Debug, "NewState_Callback not implemented");
--        raise NOT_IMPLEMENTED;
   end NewState_Callback;

   --  Event: PeerStatus
   --  Peer: SIP/2005
   --  PeerStatus: Registered
   procedure PeerStatus_Callback (Event_List : in Event_List_Type.Map) is
      use Peers.Peer_List_Type;
      use Ada.Strings.Unbounded;
      Peer        : Peer_Type;
      Map_Key     : Unbounded_String;
      Exsist      : Boolean := False;

      --  Extracts the channel type, and the phonename,
      --    and saves them in the peer
      procedure Set_PhoneInfo (Peer : in out Peer_Type;
                               Text : in Unbounded_String);

      procedure Set_PhoneInfo (Peer : in out Peer_Type;
                               Text : in Unbounded_String)
      is
         Seperator_Index : Integer;
      begin

         if Ada.Strings.Unbounded.Count (Text, "/") > 0 then
            Seperator_Index := Index (Text, "/");
            Peer.Peer := Tail (Source => Text,
                               Count  => Length (Text) - Seperator_Index);
            Peer.ChannelType := Head (Text, Seperator_Index - 1);
            if TS (Peer.ChannelType) /= "SIP" then
               Yolk.Log.Trace (Yolk.Log.Alert, TS (Peer.ChannelType));
            end if;
         else
            Yolk.Log.Trace (Yolk.Log.Debug,
                            "Set_PhoneInfo:" &
                              "This peer does not have a Channeltype: "
                            & TS (Text));
         end if;
      end Set_PhoneInfo;

   begin
      if Event_List.Contains (TUS ("Peer")) then
         Set_PhoneInfo (Peer,
                        Event_List.Element (TUS ("Peer")));
         Map_Key := Peer.Peer;
      end if;

      declare
         temp_peer : Peer_Type;
      begin
         temp_peer := Peers.Get_Peer (Map_Key);

         if temp_peer /= Peers.null_Peer then
            Peer := temp_peer;
            Exsist := True;
         end if;
         exception
         when err : others =>
            Yolk.Log.Trace (Yolk.Log.Debug, Exception_Name (err) & "|:|" &
                           Exception_Message (err));
      end;

      --  This parameter is set at the Set_PhoneInfo.
      --  if Event_List.Contains (To_Unbounded_String ("ChannelType")) then
      --      Peer.ChannelType := Event_List.Element
      --      (To_Unbounded_String ("ChannelType"));
      --  end if;

      if Event_List.Contains (TUS ("Address")) then
         Peer.Address := Event_List.Element
           (TUS ("Address"));
      end if;

      if Event_List.Contains (TUS ("Port")) then
         Peer.Port := Event_List.Element (TUS ("Port"));
      end if;

      if Event_List.Contains (TUS ("PeerStatus")) then
         if TS (Event_List.Element (TUS ("PeerStatus")))
           = "Unregistered"  then
            Peer.Status := Unregistered;
            Yolk.Log.Trace (Yolk.Log.Debug,
                            "Got peer with unregistrered status");

         elsif TS (Event_List.Element (
           TUS ("PeerStatus"))) = "Registered" then
            Peer.Status := Registered;
            Yolk.Log.Trace (Yolk.Log.Debug,
                            "Got peer with registrered status");

         else
            Yolk.Log.Trace (Yolk.Log.Debug, "Peer Status, unknown state: " &
                TS (Event_List.Element
                (TUS ("PeerStatus"))));
         end if;
      else
         Yolk.Log.Trace (Yolk.Log.Info,
                         "PeerStatus_CallbackHandler had no PeerStatus");
      end if;

      declare
         Exten : Unbounded_String;
         --  Gathering Extension.
      begin
         Exten := Peers.Get_Exten (Peer.Peer);
         if Exten = Null_Unbounded_String then
            Yolk.Log.Trace (Yolk.Log.Warning,
                            "There is not registrated any extension to agent: "
                            & TS (Peer.Peer));
            raise Program_Error;
         else
            Peer.Exten := Exten;
         end if;
      end;

      --  Update the timestamp
      Peer.Last_Seen := Ada.Calendar.Clock;

      if Exsist then
         Peers.Replace_Peer (Item => Peer);
      else
         Peers.Insert_Peer (Peer);
      end if;
   end PeerStatus_Callback;

   --  Lists the SIP peers. Returns a PeerEntry event for each
   --  SIP peer, and a PeerlistComplete event upon completetion
   --  Event: PeerEntry
   --  Channeltype: SIP
   --  ObjectName: softphone2
   --  ChanObjectType: peer
   --  IPaddress: 90.184.227.68
   --  IPport: 59028
   --  Dynamic: yes
   --  Natsupport: yes
   --  VideoSupport: no
   --  TextSupport: no
   --  ACL: no
   --  Status: Unmonitored
   --  RealtimeDevice: no
   --
   --  Event: PeerlistComplete
   --  EventList: Complete
   --  ListItems: 2
   procedure SIPPeers_Callback is
   begin
      Yolk.Log.Trace (Yolk.Log.Debug, "SipPeers_Callback not implemented");
      raise NOT_IMPLEMENTED;
   end SIPPeers_Callback;

   --  TODO: Write up and
   --    architecture that uses a queue to send requests, or blocks
   procedure Start (Channel  : in AWS.Net.Std.Socket_Type;
                    Username : in String;
                    Secret   : in String) is
      use Ada.Strings.Unbounded;
   begin
      Asterisk := (Greeting  => new String'(Read_Line (Channel)),
                   Channel   => Channel,
                   Logged_In => False);

      Yolk.Log.Trace (Yolk.Log.Debug, "Event Greetings: " &
                        Asterisk.Greeting.all);
      --  Send login
      Login (Asterisk_AMI => Asterisk,
             Username     => Username,
             Secret       => Secret);
      Yolk.Log.Trace (Yolk.Log.Debug,
                      "Ami Event logged in.");

      loop
         declare
            Event_String : constant Unbounded_String := Read_Package (Channel);
            Event_List : Event_List_Type.Map;
         begin
            Event_List := Parse (Event_String);
            --  Basically we have responses, or events
            if Event_List.Contains (TUS ("Event")) then
               begin
                  Event_Callback_Routine (AMI.Event.Event'Value (TS
                     (Event_List.Element (TUS ("Event")))))(Event_List);
               exception
                  when others =>
                     null;
               end;
            end if;
         exception
            when Error : others =>
               Yolk.Log.Trace (Yolk.Log.Debug, Exception_Name (Error));
               Yolk.Log.Trace (Yolk.Log.Debug, Exception_Message (Error));
         end;
      end loop;
   exception
      when AWS.Net.Socket_Error =>
         --  When the socket is terminated the Read_Package throws an exception
         Yolk.Log.Trace (Yolk.Log.Info, "AMI Socket Shutdown");
   end Start;

   --  Event: Unlink
   --  Privilege: call,all
   --  Channel1: SIP/softphone2-0000000a
   --  Channel2: SIP/softphone1-0000000b
   --  Uniqueid1: 1340097427.10
   --  Uniqueid2: 1340097427.11
   --  CallerID1: softphone2
   --  CallerID2: 100
   procedure Unlink_Callback (Event_List : in Event_List_Type.Map) is
   begin
      Yolk.Log.Trace (Yolk.Log.Debug, "Unlink_Callback not implemented");
      if Event_List.Contains (TUS
                              ("I'm not implemented but needed anyway")) then
         null;
      end if;
      raise NOT_IMPLEMENTED;
   end Unlink_Callback;

end AMI.Event;
