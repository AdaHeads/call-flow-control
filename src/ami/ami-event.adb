with Ada.Calendar,
     Ada.Exceptions,
     Ada.Strings.Unbounded,
     Ada.Text_IO;

with AMI.IO,
     AMI.Protocol;

with Call_Queue,
     Peers,
     Task_Controller;

with Yolk.Log;

package body AMI.Event is
   use Ada.Strings.Unbounded;
   use Call_Queue;
   use Ada.Exceptions;
   use AMI.IO;
   use Ada.Text_IO;
   use Peers;

   Asterisk         : Asterisk_AMI_Type;
   --     Peer_List        : Peer_List_Type.Map;

   --  Callback maps
   --     Callback_Routine : Action_Callback_Routine_Table :=
   --       (Login       => Login_Callback'Access,
   --        QueueStatus => QueueStatus_Callback'Access,
   --        others      => null);

   Event_Callback_Routine : constant Event_Callback_Routine_Table :=
     (Dial                 => Dial_Callback'Access,
      Hangup               => Hangup_Callback'Access,
      Join                 => Join_Callback'Access,
      PeerStatus           => PeerStatus_Callback'Access,
      Unlink               => Unlink_Callback'Access,
      others               => null);

   --  Lists agents
   procedure Agents is
   begin
      Put_Line ("Not implemented");
      raise NOT_IMPLEMENTED;
   end Agents;

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
      Put_Line ("Not implemented");
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
        Event_List.Element (To_Unbounded_String ("Message")) =
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
   begin
      if Event_List.Contains (To_Unbounded_String ("Uniqueid")) then
         Call_Queue.Remove (Event_List.Element (
           To_Unbounded_String ("Uniqueid")));
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
      Call : Call_Queue.Call_Type;
      --        Event_Key : Unbounded_String;
   begin
      --        for i in Event_List'First .. Event_List'Last loop
      --           Event_Key := Event_List (i, Key);

      if Event_List.Contains (To_Unbounded_String ("Channel")) then
         Call.Channel := Event_List.Element (To_Unbounded_String ("Channel"));
      end if;
      --           if To_String (Event_Key) = "Channel" then
      --              Call.Channel := Event_List (i, Value);

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

      Call_Queue.Enqueue (Call => Call);
   end Join_Callback;

   procedure Login (Asterisk_AMI : in Asterisk_AMI_Type;
                    Username     : in String;
                    Secret       : in String) is
      Command : constant String := AMI.Protocol.Login (Username, Secret);
   begin
      --  AMI.Action.Login (Socket   => Asterisk_AMI.Channel,
      --                    Username => Username,
      --                    Secret   => Secret);

      AMI.IO.Send (Asterisk_AMI.Channel, Command);

      --  Update the table if we were asked to used this as standard callback
      --  if Callback /= null and then Persist then
      --  Callback_Routine (Login) := Callback;
      --  end if;

   end Login;

   procedure Login_Callback (Event_List : in Event_List_Type.Map) is
   begin
      --  Now we play a game called; Find the message!
      if Event_List.Contains (To_Unbounded_String ("Message")) and
      then To_String (Event_List.Element (To_Unbounded_String ("Message"))) =
        "Authentication accepted" then
         Asterisk.Logged_In := True;
      end if;
   end Login_Callback;

   --  no need for, we are never gonna call it anyway.
   --  procedure Logoff (Asterisk_AMI : in     Asterisk_AMI_Type;
   --                    Callback     : access Callback_Type := null) is
   --
   --  begin
   --     AMI.Action.Logoff (Asterisk_AMI.Channel);
   --
   --     if Callback /= null then
   --        --  Callback;
   --        null;
   --     end if;
   --  end Logoff;

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
      Put_Line ("Not implemented");
      raise NOT_IMPLEMENTED;
   end NewState_Callback;

   --  Event: PeerStatus
   --  Peer: SIP/2005
   --  PeerStatus: Registered
   procedure PeerStatus_Callback (Event_List : in Event_List_Type.Map) is
      Peer    : Peer_Type;
      Map_Key : Unbounded_String;
   begin
      Put_Line ("Peer status update");
      --        for i in Event_List'First + 1 .. Event_List'Last loop
      if Event_List.Contains (To_Unbounded_String ("Peer")) then
         Peer.Peer := Event_List.Element (To_Unbounded_String ("Peer"));
         Map_Key := Event_List.Element (To_Unbounded_String ("Peer"));
      end if;
      if Event_List.Contains (To_Unbounded_String ("ChannelType")) then
         Peer.ChannelType := Event_List.Element
           (To_Unbounded_String ("ChannelType"));
      end if;
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
         elsif To_String (Event_List.Element (
           To_Unbounded_String ("PeerStatus"))) = "Registered"  then
            Peer.Status := Registered;
         else
            Yolk.Log.Trace (Yolk.Log.Debug, "Peer Status, unknown state: " &
                To_String (Event_List.Element
                (To_Unbounded_String ("PeerStatus"))));
            --              Put_Line ("SIP client to unknown state: " &
            --                          To_String (Event_List (i, Value)));
         end if;
      end if;
      --        end loop;

      declare
         Exten : Unbounded_String;
         --  Hent Extension.
      begin
         Exten := Peers.Get_Exten (Peer.Peer);
         if Exten = Null_Unbounded_String then
            Put_Line ("There is not registrated any extension to agent: " &
                        To_String (Peer.Peer));
            raise Program_Error;
         else
            Put_Line ("Peer got Exten => " & To_String (Exten));
            Peer.Exten := Exten;
         end if;
      end;

      --  Update the timestamp
      Peer.Last_Seen := Ada.Calendar.Clock;
      declare
         use Peers.Peer_List_Type;
         Peer_Cursor : Peer_List_Type.Cursor;
      begin
         Peer_Cursor := Peer_List_Type.Find (Peers.Get_Peers_List,
                                             Map_Key);
         if Peer_Cursor /= Peer_List_Type.No_Element then
            Peers.Replace_Peer (Item => Peer);
         else
            Peers.Insert_Peer (Peer);
         end if;
      end;
      --  Update the peer list
      --        if Peer_List_Type.Contains (Container => Peers.Get_Peers_List,
      --                                    Key       => Map_Key) then

      --           Peer_List_Type.Replace (Container => Peer_List,
      --                                   Key       => Map_Key,
      --                                   New_Item  => Peer);
      --        else
      --
      --           Peer_List_Type.Insert (Container => Peer_List,
      --                                  Key       => Map_Key,
      --                                  New_Item  => Peer);
      --        end if;

      --        Print_Peer (Peer_List_Type.Element (Container => Peer_List,
      --                                            Key       => Map_Key));
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
      Put_Line ("Not implemented");
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
               Put_Line (Ada.Exceptions.Exception_Name (Error));
               Put_Line (Exception_Message (Error));
               Put_Line ("Socket.Start.declare: ");
         end;
      end loop;
   exception
      when AWS.Net.Socket_Error =>
         --  When the socket is terminated the Read_Package throws an exception
         Put_Line ("AMI Socket Shutdown");
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
      Put_Line ("Not implemented " &
          To_String (Event_List.Element (
          To_Unbounded_String ("Event"))));
      raise NOT_IMPLEMENTED;
   end Unlink_Callback;

end AMI.Event;
