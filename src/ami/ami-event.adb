with Ada.Exceptions;  use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar;
with Ada.Containers;
with AMI.IO; use AMI.IO;
with Task_Controller;
with AMI.Protocol;
with Yolk.Log;
with Call_Queue;
with Peers;
package body AMI.Event is
   use Call_Queue;
--     use AMI.Action;
   use Peers;

   Asterisk         : Asterisk_AMI_Type;
--     Peer_List        : Peer_List_Type.Map;
   Consistency      : Queue_Type.Vector;

   --  Callback maps
--     Callback_Routine : Action_Callback_Routine_Table :=
--       (Login       => Login_Callback'Access,
--        QueueStatus => QueueStatus_Callback'Access,
--        others      => null);

   Event_Callback_Routine : constant Event_Callback_Routine_Table :=
     (Dial                 => Dial_Callback'Access,
      Hangup               => Hangup_Callback'Access,
      Join                 => Join_Callback'Access,
      QueueMemberPaused    => QueueMemberPaused_Callback'Access,
      PeerStatus           => PeerStatus_Callback'Access,
      Unlink               => Unlink_Callback'Access,
      QueueStatusComplete  => QueueStatusComplete_CallBack'Access,
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
   procedure Dial_Callback (Event_List : in Event_List_Type) is
   begin
      --  Now we play a game called; Find the message!
      for i in Event_List'First + 1 .. Event_List'Last loop
         if To_String (Event_List (i, Key)) = "Message" and
         then To_String (Event_List (i, Value)) =
           "Authentication accepted" then
            Asterisk.Logged_In := True;
         end if;
      end loop;
   end Dial_Callback;

   --  Event: Hangup
   --  Privilege: call,all
   --  Channel: SIP/softphone1-0000000b
   --  Uniqueid: 1340097427.11
   --  CallerIDNum: 100
   --  CallerIDName: <unknown>
   --  Cause: 16
   --  Cause-txt: Normal Clearing
   procedure Hangup_Callback (Event_List : in Event_List_Type) is
   begin
      --  Search for the right key in Event_List
      for i in Event_List'Range loop
         if To_String (Event_List (i, Key)) = "Uniqueid" then
            --  If there should happen more then just remove it,
            --   then remember to insert Ended time.
            Call_Queue.Remove (Uniqueid => Event_List (i, Value));

            --  The field is found, and we are all happy now.
            return;
         end if;
      end loop;
   end Hangup_Callback;

   --  Event: Join
   --  Channel: SIP/TP-Softphone-00000021
   --  CallerIDNum: TP-Softphone
   --  CallerIDName: unknown
   --  Queue: testqueue1
   --  Position: 1
   --  Count 1
   --  Uniqueid: 1340807150.33
   procedure Join_Callback (Event_List : in Event_List_Type) is
      Call : Call_Queue.Call_Type;
      Event_Key : Unbounded_String;
   begin
      for i in Event_List'First .. Event_List'Last loop
         Event_Key := Event_List (i, Key);

         if To_String (Event_Key) = "Channel" then
            Call.Channel := Event_List (i, Value);

         elsif To_String (Event_Key) = "CallerIDNum" then
            Call.CallerIDNum := Event_List (i, Value);

         elsif To_String (Event_Key) = "CallerIDName" then
            Call.CallerIDName := Event_List (i, Value);

         elsif To_String (Event_Key) = "Queue" then
            Call.Queue := Event_List (i, Value);

         elsif To_String (Event_Key) = "Position" then
            Call.Position := Integer'Value (To_String (Event_List (i, Value)));

         elsif To_String (Event_Key) = "Count" then
            Call.Count := Integer'Value (To_String (Event_List (i, Value)));

         elsif To_String (Event_Key) = "Uniqueid" then
            Call.Uniqueid := Event_List (i, Value);
         end if;
      end loop;
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

   procedure Login_Callback (Event_List : in Event_List_Type) is
   begin
      --  Now we play a game called; Find the message!
      for i in Event_List'First + 1 .. Event_List'Last loop
         if To_String (Event_List (i, Key)) = "Message" and
         then To_String (Event_List (i, Value)) =
           "Authentication accepted" then
            Asterisk.Logged_In := True;
         end if;
      end loop;
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
   procedure PeerStatus_Callback (Event_List : in Event_List_Type) is
      Peer    : Peer_Type;
      Map_Key : Unbounded_String;
   begin
      Put_Line ("Peer status update");
      for i in Event_List'First + 1 .. Event_List'Last loop
         if To_String (Event_List (i, Key)) = "Peer" then
            Peer.Peer := Event_List (i, Value);
            Map_Key := Event_List (i, Value);

         elsif To_String (Event_List (i, Key)) = "ChannelType" then
            Peer.ChannelType := Event_List (i, Value);

         elsif To_String (Event_List (i, Key)) = "Address" then
            Peer.Address := Event_List (i, Value);

         elsif To_String (Event_List (i, Key)) = "Port" then
            Peer.Port := Event_List (i, Value);

         elsif To_String (Event_List (i, Key)) = "PeerStatus" then
            if To_String (Event_List (i, Value)) = "Unregistered"  then
               Peer.Status := Unregistered;
            elsif To_String (Event_List (i, Value)) = "Registered"  then
               Peer.Status := Registered;
            else
               Put_Line ("SIP client to unknown state: " &
                           To_String (Event_List (i, Value)));
            end if;
         end if;
      end loop;

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

   --  Event: QueueMemberPaused
   --  Privilege: agent,all
   --  Queue: myqueue
   --  Location: SIP/testphone
   --  MemberName: Jared Smith
   --  Paused: 1
   procedure QueueMemberPaused_Callback (Event_List : in Event_List_Type) is
--        Peer_Phone : Unbounded_String;
--        paused : Boolean := False;
--        Peer_Cursor : Peer_List_Type.Cursor;
--        Peer : Peer_Type;
   begin
--        for i in Event_List'Range loop
--           if To_String (Event_List (i, Key)) = "Location" then
--              Peer_Phone := Event_List (i, Value);
--
--           elsif To_String (Event_List (i, Key)) = "Paused" then
--              if To_String (Event_List (i, Value)) = "0" then
--                 paused := False;
--
--              elsif To_String (Event_List (i, Value)) = "1" then
--                 paused := True;
--              end if;
--           end if;
--        end loop;

--        Peer_Cursor :=  Peer_List_Type.Find (Peer_List, Peer_Phone);
--        Peer := Peer_List_Type.Element (Peer_Cursor);
--
--        Peer.Paused := paused;
--        Peer_List_Type.Replace_Element (Container => Peer_List,
--                                        Position => Peer_Cursor,
--                                        New_Item => Peer);

      Put_Line ("Not implemented - QueueMemberPaused_Callback" &
                  To_String (Event_List (1, Value)));
      --  raise NOT_IMPLEMENTED;
   end QueueMemberPaused_Callback;

   --  Response: Success
   --  Message: Queue status will follow
   procedure QueueStatus_Callback (Event_List : in Event_List_Type) is
   begin
      for i in Event_List'Range loop
         if Event_List (i, Key) = "Message" then
            if Event_List (i, Value) = "Queue status will follow" then
               Consistency.Clear;
            end if;
         end if;
      end loop;
   end QueueStatus_Callback;

   procedure QueueStatusComplete_CallBack (Event_List : in Event_List_Type) is
      use Ada.Containers;
      Queue : constant Call_Queue_Type := Call_Queue.Get_Queue;
      function Check_Call (Call : in Call_Type) return Boolean;
      function Check_Call (Call : in Call_Type) return Boolean is
      begin
         for Queue_Priority in Queue'Range loop
            for Queue_Index in
              1 .. Integer (Queue (Queue_Priority).Length) loop
               if Call.Uniqueid =
                 Queue (Queue_Priority).Element (Queue_Index).Uniqueid
               then
                  return True;
               end if;
            end loop;
         end loop;
         return False;
      end Check_Call;
   begin
      for i in Event_List'Range loop
         if To_String (Event_List (i, Key)) = "ActionID" then
            if To_String (Event_List (i, Value)) = "Consistency" then
               Put_Line ("Consistency Check");
               if Call_Queue.Queue_Length /= Consistency.Length then
                  Put_Line ("-----------------------------------------------");
                  Put_Line ("         Consistency check - Length failed     ");
                  Put_Line ("Call Queue Length: " &
                              Call_Queue.Queue_Length'Img);
                  Put_Line ("Asterisk Queue Length: " &
                              Consistency.Length'Img);
                  Put_Line ("-----------------------------------------------");
               end if;
               --  TODO Error Correction
               --  XXX Der er en chance for det her er langsomst.

               for Cons_Index in 1 .. Integer (Consistency.Length) loop
                  if not Check_Call (Consistency.Element (Cons_Index)) then
                     Put_Line ("--------------------------------------------");
                     Put_Line ("    Consistency check - Not Equal failed    ");
                     Put_Line (Call_To_String
                               (Consistency.Element (Cons_Index)));
                     Put_Line ("Does not exsist in our call queue");
                     Put_Line ("--------------------------------------------");
                  end if;
               end loop;
               Put_Line ("The system is consistent");
            end if;
         end if;
      end loop;

   end QueueStatusComplete_CallBack;

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
      --  AMI.Action.QueueStatus (Asterisk.Channel, "StartUp");

      loop
         exit when Task_State = Down;
         declare
            Event_String : constant Unbounded_String := Read_Package (channel);
            Event_List : constant Event_List_Type := Parse (Event_String);
         begin
            --  Basically we have responses, or events
            if Event_List (Event_List'First, Key)  = "Event" then
               begin
                  Event_Callback_Routine
                    (AMI.Event.Event'Value (To_String
                     (Event_List (Event_List'First, Value)))) (Event_List);
               exception
                  when others =>
                     null;
                     --  when Error : others =>
                     --  Put_Line ("Event not implemented: " &
                     --    To_String (Event_List (1, Value)));
                     --  Put_Line (Exception_Message (Error));
               end;

--              elsif Event_List (Event_List'First, Key)  = "Response" then
--                 --  Lookup the callback, and pass the value.
--                 Callback_Routine (AMI.Action.Get_Last_Action)(Event_List);
--                 --  Direct it to the callback associated
--                 --    with the previous commmand
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
         Put_Line ("AMI Socket Shutdowned");
   end Start;

   --  Event: Unlink
   --  Privilege: call,all
   --  Channel1: SIP/softphone2-0000000a
   --  Channel2: SIP/softphone1-0000000b
   --  Uniqueid1: 1340097427.10
   --  Uniqueid2: 1340097427.11
   --  CallerID1: softphone2
   --  CallerID2: 100
   procedure Unlink_Callback (Event_List : in Event_List_Type) is
   begin
      Put_Line ("Not implemented " &
                  To_String (Event_List (Event_List'First, Value)));
      raise NOT_IMPLEMENTED;
   end Unlink_Callback;

end AMI.Event;
