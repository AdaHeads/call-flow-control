with Ada.Calendar,
     Ada.Exceptions,
     Ada.Strings.Unbounded;

with AMI.Action,
     AMI.IO,
     AMI.Protocol;

with Call_List,
     Peers,
     Task_Controller;

with Yolk.Log;

package body AMI.Event is
   use Ada.Strings.Unbounded;
   use Call_List;
   use Ada.Exceptions;
   use AMI.IO;
   use Peers;

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
   begin
      if Event_List.Contains (To_Unbounded_String ("Message")) and then
        Event_List.Element  (To_Unbounded_String ("Message")) =
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
   begin
      if Event_List.Contains (To_Unbounded_String ("Uniqueid")) then
         Call := Call_List.Remove (Event_List.Element (
           To_Unbounded_String ("Uniqueid")));
         Yolk.Log.Trace (Yolk.Log.Debug, "This call have been hangup: " &
                           "Channel: " & To_String (Call.Channel) &
                           "UniqueID: " & To_String (Call.Uniqueid));
      end if;
   end Hangup_Callback;

   --  Event: Join
   --  Channel: SIP/TP-Softphone-00000021
   --  CallerIDNum: TP-Softphone
   --  CallerIDName: unknown
   --  Queue: testqueue1
   --  Position: 1
   --  Count 1
   --  Uniqueid: 1340807150.33
   procedure Join_Callback (Event_List : in Event_List_Type.Map) is
      Call : Call_List.Call_Type;
   begin
      if Event_List.Contains (To_Unbounded_String ("Channel")) then
         Call.Channel := Event_List.Element (To_Unbounded_String ("Channel"));
      end if;

      if Event_List.Contains (To_Unbounded_String ("CallerIDNum")) then
         Call.CallerIDNum := Event_List.Element
           (To_Unbounded_String ("CallerIDNum"));
      end if;
      if Event_List.Contains (To_Unbounded_String ("CallerIDName")) then
         Call.CallerIDName := Event_List.Element
           (To_Unbounded_String ("CallerIDName"));
      end if;
      if Event_List.Contains (To_Unbounded_String ("Queue")) then
         Call.Queue := Event_List.Element (To_Unbounded_String ("Queue"));
      end if;
      if Event_List.Contains (To_Unbounded_String ("Position")) then
         Call.Position := Integer'Value (To_String (
           Event_List.Element (To_Unbounded_String ("Position"))));
      end if;
      if Event_List.Contains (To_Unbounded_String ("Count")) then
         Call.Count := Integer'Value (To_String (
           Event_List.Element (To_Unbounded_String ("Count"))));
      end if;
      if Event_List.Contains (To_Unbounded_String ("Uniqueid")) then
         Call.Uniqueid := Event_List.Element
           (To_Unbounded_String ("Uniqueid"));
      end if;
      Call.Arrived := Ada.Calendar.Clock;

      declare
         State : Unbounded_String;
      begin
         AMI.Action.Action_Manager.Get_Var
           (Channel      => To_String (Call.Channel),
            VariableName => "CallState",
            Value        => State);

         if To_String (State) = "(null)" or else To_String (State) = "" then
            Call_List.Add (Call => Call);
         end if;
      end;
   end Join_Callback;

   procedure Login (Asterisk_AMI : in Asterisk_AMI_Type;
                    Username     : in String;
                    Secret       : in String) is
      Command : constant String := AMI.Protocol.Login (Username, Secret);
   begin
      AMI.IO.Send (Asterisk_AMI.Channel, Command);

   end Login;

   procedure Login_Callback (Event_List : in Event_List_Type.Map) is
      Message : Unbounded_String;
   begin

      if Event_List.Contains (To_Unbounded_String ("Message")) then
         Message := Event_List.Element
           (To_Unbounded_String ("Message"));

         if To_String (Message) =  "Authentication accepted" then
            Asterisk.Logged_In := True;
         else
            Asterisk.Logged_In := False;
         end if;
      end if;

   end Login_Callback;

   procedure Newchannel_Callback (Event_List : in Event_List_Type.Map) is
   begin
      null;
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
      raise NOT_IMPLEMENTED;
   end NewState_Callback;

   --  Event: PeerStatus
   --  Peer: SIP/2005
   --  PeerStatus: Registered
   procedure PeerStatus_Callback (Event_List : in Event_List_Type.Map) is
      use Peers.Peer_List_Type;
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
            if To_String (Peer.ChannelType) /= "SIP" then
               Yolk.Log.Trace (Yolk.Log.Alert, To_String (Peer.ChannelType));
            end if;
         else
            Yolk.Log.Trace (Yolk.Log.Debug,
                            "Set_PhoneInfo:" &
                              "This peer does not have a Channeltype: "
                            & To_String (Text));
         end if;
      end Set_PhoneInfo;

   begin
      if Event_List.Contains (To_Unbounded_String ("Peer")) then
         Set_PhoneInfo (Peer,
                        Event_List.Element (To_Unbounded_String ("Peer")));
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

      if Event_List.Contains (To_Unbounded_String ("Address")) then
         Peer.Address := Event_List.Element
           (To_Unbounded_String ("Address"));
      end if;

      if Event_List.Contains (To_Unbounded_String ("Port")) then
         Peer.Port := Event_List.Element (To_Unbounded_String ("Port"));
      end if;

      if Event_List.Contains (To_Unbounded_String ("PeerStatus")) then
         if To_String (Event_List.Element (To_Unbounded_String ("PeerStatus")))
           = "Unregistered"  then
            Peer.Status := Unregistered;
            Yolk.Log.Trace (Yolk.Log.Debug,
                            "Got peer with unregistrered status");

         elsif To_String (Event_List.Element (
           To_Unbounded_String ("PeerStatus"))) = "Registered" then
            Peer.Status := Registered;
            Yolk.Log.Trace (Yolk.Log.Debug,
                            "Got peer with registrered status");

         else
            Yolk.Log.Trace (Yolk.Log.Debug, "Peer Status, unknown state: " &
                To_String (Event_List.Element
                (To_Unbounded_String ("PeerStatus"))));
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
                            & To_String (Peer.Peer));
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
   procedure Start (channel  : in AWS.Net.Std.Socket_Type;
                    Username : in String;
                    Secret   : in String) is
      use Task_Controller;

   begin
      Asterisk := (Greeting  => new String'(Read_Line (channel)),
                   Channel   => channel,
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
         exit when Task_State = Down;
         declare
            Event_String : constant Unbounded_String := Read_Package (channel);
            Event_List : constant Event_List_Type.Map := Parse (Event_String);
         begin
            --  Basically we have responses, or events
            if Event_List.Contains (To_Unbounded_String ("Event")) then
               begin
                  Event_Callback_Routine
                    (AMI.Event.Event'Value (To_String
                     (Event_List.Element
                        (To_Unbounded_String ("Event")))))(Event_List);
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
      if Event_List.Contains (To_Unbounded_String
                              ("I'm not implemented but needed anyway")) then
         null;
      end if;
      raise NOT_IMPLEMENTED;
   end Unlink_Callback;

end AMI.Event;
