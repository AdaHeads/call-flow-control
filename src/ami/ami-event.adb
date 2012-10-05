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

with Ada.Calendar;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with AMI.IO;
with AMI.Protocol;
with Call_List;
with Errors;
with Peers;
with Yolk.Log;

package body AMI.Event is
--  use Ada.Strings.Unbounded;
   use Call_List;
   use Ada.Exceptions;
   use AMI.IO;
   use Peers;
   use Yolk.Log;

   function TS
     (US : in Ada.Strings.Unbounded.Unbounded_String)
      return String
      renames Ada.Strings.Unbounded.To_String;

--     function TUS
--       (S : in String)
--        return Ada.Strings.Unbounded.Unbounded_String
--        renames Ada.Strings.Unbounded.To_Unbounded_String;

   Asterisk : Asterisk_AMI_Type;

   Event_Callback_Routine : constant Event_Callback_Routine_Table :=
     (Dial                 => Dial_Callback'Access,
      Hangup               => Hangup_Callback'Access,
      Join                 => Join_Callback'Access,
      Newchannel           => Newchannel_Callback'Access,
      PeerStatus           => PeerStatus_Callback'Access,
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
   --  use Ada.Strings.Unbonded;
   --  Temp_Value : Unbounded_String;
   begin
      null;
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
      use Ada.Strings.Unbounded;
      Call       : Call_Type;
      Uniqueid   : Unbounded_String;
      Temp_Value : Unbounded_String;
   begin
      if Try_Get (Event_List, Event_Parser.Uniqueid, Temp_Value) then
         Uniqueid := Temp_Value;
         Call := Call_List.Remove (Uniqueid);

         Yolk.Log.Trace (Yolk.Log.Debug, "This call have been hangup: " &
                           "Channel: " & TS (Call.Channel) &
                           "UniqueID: " & TS (Call.Uniqueid));
         if Call = Null_Call then
            Yolk.Log.Trace
              (Yolk.Log.Info,
               "Got a hangup on a call, that was not in the Calls list." &
                 " Uniqueid: " & TS (Uniqueid));
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
      use Ada.Strings.Unbounded;

      Call : Call_List.Call_Type := Null_Call;
      --  Unknownen
      Temp_Value : Unbounded_String;
   begin
      Trace (Debug, "------------- JOIN EVENT-------------");
      if Try_Get (Event_List, Uniqueid, Temp_Value) then
         --  The call should exsists at this point.
         Call := Call_List.Get_Call (Temp_Value);
      end if;

      --  There are no call with that ID, something is wrong.
      if Call = Null_Call then
         Trace (Debug, "------ JOIN EVENT --- NO CALL -----");
         if Try_Get (Event_List, Channel, Temp_Value) then
            Trace (Error,
                   "Got a Join evnet, " &
                     "on a call there is not in the call list. " &
                     "Channel: " &
                     TS (Temp_Value));
         else
            Trace (Error,
                   "Got a Join Event, on a call that don't exsist" &
                     " and do not have a Channel");
         end if;
         return;
      end if;

      if Call.State = Call_List.Unknown then
         Trace (Debug, "------- JOIN EVENT --- Call unknown state ------");
         Call.State := Call_List.Queued;

         if Try_Get (Event_List, Queue, Temp_Value) then
            Call.Queue := Temp_Value;
         end if;

      elsif Call.State = Call_List.OnHold then
         null;
      else
         Yolk.Log.Trace (Yolk.Log.Error, "Join Event, Call with bad state: " &
                           Call.State'Img);
      end if;
      Trace (Debug, "------- JOIN EVENT --- Call Upadete ------");
      Call_List.Update (Call);
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
      Temp_Value : Unbounded_String;
   begin
      if Try_Get (Event_List, Event_Parser.Message, Temp_Value) then
         Message := Temp_Value;

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
      use Ada.Strings.Unbounded;

      Call       : Call_Type;
      Temp_Value : Unbounded_String := Null_Unbounded_String;
   begin

      if Try_Get (Event_List, Channel, Temp_Value) then
         Call.Channel := Temp_Value;
      end if;

      if Try_Get (Event_List, Uniqueid, Temp_Value) then
         Call.Uniqueid := Temp_Value;
      end if;

      if Try_Get (Event_List, Exten, Temp_Value) then
         Call.Extension := Temp_Value;
      end if;

      --  Save the time when the call came in.
      Call.Arrived := Ada.Calendar.Clock;
      Call.State := Unknown;
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
      Peer    : Peer_Type;
      Map_Key : Unbounded_String;

      --  Extracts the channel type, and the phonename,
      --    and saves them in the peer. Format: ChannelType/phonename
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

      Temp_Value : Unbounded_String;
   begin
      if Try_Get (Event_List, Event_Parser.Peer, Temp_Value) then
         Set_PhoneInfo (Peer, Temp_Value);
         Map_Key := Peer.Peer;
      end if;

      declare
         Temp_Peer : Peer_Type;
      begin
         Temp_Peer := Peers.Get_Peer_By_PhoneName (Map_Key);

         if Temp_Peer /= Peers.Null_Peer then
            --  Update the timestamp
            Peer.Last_Seen := Ada.Calendar.Clock;
            Peer := Temp_Peer;
            Peers.Replace_Peer (Item => Peer);
            return;

         else
            Trace (Debug, "Peer not found: [" & TS (Map_Key) & "]");
            Trace (Debug, Peers.List_As_String);
         end if;
      exception
         when Err : others =>
            Yolk.Log.Trace (Yolk.Log.Debug, Exception_Name (Err) & "|:|" &
                              Exception_Message (Err));
      end;

      if Try_Get (Event_List, Address, Temp_Value) then
         Peer.Address := Temp_Value;
      end if;

      if Try_Get (Event_List, Port, Temp_Value) then
         Peer.Port := Temp_Value;
      end if;

      --  Setting the State - Registrated or not.
      if Try_Get (Event_List, PeerStatus, Temp_Value) then
         if TS (Temp_Value) = "Unregistered"  then
            Peer.Status := Unregistered;
            Yolk.Log.Trace (Yolk.Log.Debug,
                            "Got peer: " & TS (Peer.Peer) &
                              " with unregistrered status");

         elsif TS (Temp_Value) = "Registered" then
            Peer.Status := Registered;
            Yolk.Log.Trace (Yolk.Log.Debug,
                            "Got peer: " & TS (Peer.Peer) &
                              " with registrered status");

         else
            Yolk.Log.Trace (Yolk.Log.Debug, "Peer Status, unknown state: " &
                TS (Temp_Value));
         end if;
      else
         Yolk.Log.Trace (Yolk.Log.Info,
                         "PeerStatus_CallbackHandler had no PeerStatus");
      end if;

      --  Gathering Extension.
      declare
         Exten : Unbounded_String;
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
      --  Insert Timestamp
      Peer.Last_Seen := Ada.Calendar.Clock;
      Peers.Insert_Peer (Peer);
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
   procedure Start (Socket   : in AWS.Net.Std.Socket_Type;
                    Username : in String;
                    Secret   : in String) is
      use Ada.Strings.Unbounded;

      procedure Dispatch_Event (Event_List : in Event_List_Type.Map;
                                Event_Name : in Unbounded_String);

      procedure Dispatch_Event (Event_List : in Event_List_Type.Map;
                                Event_Name : in Unbounded_String) is
         --  TODO find a good name for this variable.
         Event_Something : Event;
      begin
         begin
            Event_Something := AMI.Event.Event'Value (TS (Event_Name));
         exception
            when Constraint_Error =>
               Trace (Error, "Unknown Event: " & To_String (Event_Name));
         end;

         Event_Callback_Routine (Event_Something) (Event_List);
      exception
         when Err : others =>
            if Event_Callback_Routine (Event_Something) = null then
               Trace (Info, "Unhandled Event: " & Event_Something'Img);
            else
               Errors.Log_Exception (Err, "Event_Name: " &
                                    To_String (Event_Name));
            end if;
      end Dispatch_Event;

   begin
      Asterisk := (Greeting  => new String'(Read_Line (Socket)),
                   Channel   => Socket,
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
            Event_String : constant Unbounded_String := Read_Package (Socket);
            Event_List   : Event_List_Type.Map;
            Temp_Value   : Unbounded_String;
         begin
            Event_List := Parse (Event_String);
            if Try_Get (Event_List, Event_Parser.Event, Temp_Value) then
               Dispatch_Event (Event_List, Temp_Value);
            end if;
         exception
            when Error : others =>
               Trace (Debug, "---DEBUG - Event Exception---");
               Trace (Debug, Exception_Name (Error));
               Trace (Debug, Exception_Message (Error));
         end;
      end loop;
   end Start;

end AMI.Event;
