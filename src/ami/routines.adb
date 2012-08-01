with AMI.Action,
     Ada.Calendar,
     Ada.Containers,
     Ada.Exceptions;

with Peers;
with Event_Parser;
with Yolk.Log;

package body Routines is
      --  Takes two channels, and bridge the them together.
   procedure Bridge_Call (Channel1 : in Unbounded_String;
                          Channel2 : in Unbounded_String) is
   begin
      --  TODO, Jeg er ikke sikker, at jeg bare kan hive Asterisk her
      AMI.Action.Action_Manager.Bridge
        (To_String (Channel1), To_String (Channel2));
   end Bridge_Call;

   procedure Consistency_Check is
      use Call_Queue;
      use Ada.Containers;

      Call_List : AMI.Action.Call_List.Vector;

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
      AMI.Action.Action_Manager.QueueStatus (Call_List);
      --  AMI.Action.QueueStatus (Asterisk.Channel, "Consistency");
      Yolk.Log.Trace (Yolk.Log.Debug, "Consistency Check");
      if Call_Queue.Queue_Length /= Call_List.Length then
         Yolk.Log.Trace (Yolk.Log.Info,
         "Consistency check - Length failed" &
         "Call Queue Length: " & Call_Queue.Queue_Length'Img &
         "Asterisk Queue Length: " & Call_List.Length'Img);
      end if;
      --  TODO Error Correction
      --  XXX Der er en chance for det her er langsomst.

      for Cons_Index in 1 .. Integer (Call_List.Length) loop
         if not Check_Call (Call_List.Element (Cons_Index)) then
            Yolk.Log.Trace (Yolk.Log.Info,
                            "Consistency check - Not Equal failed" &
                            Call_To_String (Call_List.Element (Cons_Index)) &
                            "Does not exsist in our call queue");
         end if;
      end loop;
      Yolk.Log.Trace (Yolk.Log.Debug, "The system is consistent");
   end Consistency_Check;

   --  Get the specific call with UniqueId matching
   --  If unitqueID is null, then the first call in the queue is taken.
   procedure Get_Call (Uniqueid : in     String;
                       Agent    : in     String;
                       Call     :    out Call_Queue.Call_Type) is
      use Call_Queue;
      use Peers;
      use Peers.Peer_List_Type;
      temp_Call : Call_Type;
      Peer : Peer_Type;
      Peer_List : constant Peer_List_Type.Map := Peers.Get_Peers_List;
      Peer_List_Index : constant Peer_List_Type.Cursor :=
        Peer_List_Type.Find
          (Peer_List, To_Unbounded_String (Agent));
   begin
      --  Check if there exsist an Agent by that name.
      if Peer_List_Index = Peer_List_Type.No_Element then
         Yolk.Log.Trace (Yolk.Log.Debug, "Get_Call: " &
                           "We have no agent registred by the name: <" &
                           Agent & ">");

         Call := null_Call;
         return;
      end if;

      --  Now that we know that there exists an agent,
      --   is the SIP phone registreted
      Peer := Peer_List_Type.Element (Peer_List_Index);
      Yolk.Log.Trace (Yolk.Log.Debug, "I got an Peer in Gel_Call on address: "
                      & To_String (Peer.Address));
      if Peer.Status = Unregistered then
         Yolk.Log.Trace (Yolk.Log.Debug, "Get_Call: " &
                           "The following agent is unregistred: " & Agent);
         raise Program_Error;
      end if;

      --   --  TESTING
      --   Put (To_String (Call_Queue.Queue_ToString));
      --   --  TESTING

      --  If Uniqueueid parameter is null,
      --   then take the next call in the call queue.
      if Uniqueid = "" then
         Call_Queue.Dequeue (Call => temp_Call);
      else
         Call_Queue.Dequeue (Uniqueid => To_Unbounded_String (Uniqueid),
                             Call     => temp_Call);
      end if;

      --  If there is a call to anwser.
      if temp_Call /= Call_Queue.null_Call then
         --  --  TESTING
         --  Put_Line ("Got Call");
         --  Call_Queue.printCall (Call => temp_Call);
         --  --  TESTING

         temp_Call.Picked_Up := Ada.Calendar.Clock;
         temp_Call.Is_Picked_Up := True;

         Peer.Call := temp_Call;
         Peers.Replace_Peer (Peer);

         Call := temp_Call;

         --  Send the call out to the phone
         AMI.Action.Action_Manager.Redirect
           (Channel => To_String (temp_Call.Channel),
            Exten   => To_String (Peer.Exten),
            Context => "LocalSets");
      else
         Yolk.Log.Trace (Yolk.Log.Debug, "Get_Call: No Call to take");

         Call := null_Call;
      end if;
   exception
      when Error : others =>
         Yolk.Log.Trace (Yolk.Log.Debug,
                        "Got and Exception in Get_Call");
         Yolk.Log.Trace (Yolk.Log.Debug,
                         Ada.Exceptions.Exception_Information (Error));
         Call := null_Call;
   end Get_Call;

      --  Scaffolding
   function Get_Version return String is
      use Event_Parser;
      Data : Event_List_Type.Map;
      Version : Unbounded_String;
   begin
      AMI.Action.Action_Manager.CoreSettings (Data);
      if Data.Contains (To_Unbounded_String ("AsteriskVersion")) then
         Version := Data.Element (To_Unbounded_String ("AsteriskVersion"));
      end if;
      AMI.Action.Action_Manager.Ping;
      return To_String (Version);
      --        Last_Action := CoreSettings;
   end Get_Version;

      --  Sends the agent's current call on hold / park
   procedure Park (Agent : in Unbounded_String) is
      use Peers;
      use Peers.Peer_List_Type;
      use Call_Queue;
      Peer_Cursor : Peer_List_Type.Cursor;
   begin
      --  Finds the Agent, to get the call to park.
      Peer_Cursor := Peer_List_Type.Find (Container => Peers.Get_Peers_List,
                                          Key       => Agent);

      if Peer_Cursor /= Peer_List_Type.No_Element then
         declare
            Peer : Peer_Type;
         begin
            Peer := Peer_List_Type.Element (Peer_Cursor);
            --  Not sure about this.
            Yolk.Log.Trace (Yolk.Log.Debug, "Park: Peer => " &
                              To_String (Peer.Peer));
            if Peer.Call /= null_Call then
               AMI.Action.Action_Manager.Park
                 (Channel1 => To_String (Peer.Call.Channel),
                  Channel2 => To_String (Peer.Peer));
            else
               Yolk.Log.Trace (Handle => Yolk.Log.Debug,
                               Message => "This agent have no call: " &
                                 To_String (Agent));
               raise AMI.NOT_IMPLEMENTED;
            end if;
         end;
      else
         raise AMI.NOT_IMPLEMENTED;
      end if;
   end Park;

   procedure Register_Agent (PhoneName   : in Unbounded_String;
                             Computer_ID : in Unbounded_String) is
      use Peers;
      Peer : Peer_Type;
      Peer_Index : Peer_List_Type.Cursor;
      Peer_List : constant Peer_List_Type.Map := Peers.Get_Peers_List;
   begin
      Peer_Index := Peer_List_Type.Find (Container => Peer_List,
                                         Key       => PhoneName);
      if Peer_List_Type.Has_Element (Peer_Index) then
         --  The phone allready exsist in the list.
         Peer := Peer_List_Type.Element (Position => Peer_Index);
         Peer.Computer_ID := Computer_ID;
         Peers.Replace_Peer (Item => Peer);
      else
         Peer.Peer := PhoneName;
         Peer.Computer_ID := Computer_ID;
         Peers.Insert_Peer (New_Item  => Peer);
      end if;
   end Register_Agent;

   procedure StartUpSequence is
      Call_List : AMI.Action.Call_List.Vector;
   begin

      AMI.Action.Action_Manager.QueueStatus (Call_List);
      for i in Call_List.First_Index .. Call_List.Last_Index loop
         Call_Queue.Enqueue (Call => Call_List.Element (i));
      end loop;
   end StartUpSequence;

   procedure TEST_StatusPrint is
      use Peers;
   begin
      Yolk.Log.Trace (Yolk.Log.Debug, "Peers");
      for Peer in Peers.Get_Peers_List.Iterate loop
         Yolk.Log.Trace (Yolk.Log.Debug, To_String
                         (Peer_List_Type.Element (Peer).Peer));
--           Peers.Print_Peer (Peer_List_Type.Element (Peer));
      end loop;

--        Put (To_String (Call_Queue.Queue_ToString));
--        Put_Line ("Queue length" & Integer (Call_Queue.Queue_Length)'Img);
   end TEST_StatusPrint;
end Routines;
