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
                       Call     :    out Call_Queue.Call_Type;
                       Status   :    out Status_Type) is
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
      Status := Unknowen_Error;
      --  Check if there exsist an Agent by that name.
      if Peer_List_Index = Peer_List_Type.No_Element then
         Yolk.Log.Trace (Yolk.Log.Debug, "Get_Call: " &
                           "We have no agent registred by the name: [" &
                           Agent & "]");
         Status := No_Agent_Found;
         Call := null_Call;
         return;
      end if;

      --  Now that we know that there exists an agent,
      --   is the SIP phone registreted?
      Peer := Peer_List_Type.Element (Peer_List_Index);
      Yolk.Log.Trace (Yolk.Log.Debug, "I got an Peer in Get_Call on address: "
                      & To_String (Peer.Address));
      if Peer.Status = Unregistered then
         Yolk.Log.Trace (Yolk.Log.Debug, "Get_Call: " &
                           "The following agent is unregistred: " & Agent);
         Status := Unregistred_Agent;
         return;
      end if;

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
         Status := Success;
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
         Status := No_Call_Found;
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
      return To_String (Version);
   end Get_Version;

   procedure Hangup (Agent  : in     Unbounded_String;
                     Status :    out Status_Type) is
      use Peers;
      use Call_Queue;

      Peer : Peer_Type;
   begin
      Yolk.Log.Trace (Yolk.Log.Debug, "Hangup: routine started");
      Status := Unknowen_Error;
      Peer := Peers.Get_Peer (Agent);

      if Peer = Peers.null_Peer then
         Yolk.Log.Trace (Yolk.Log.Debug,
                         "Hangup: No agent found by that name: " &
                           To_String (Agent));
         Status := No_Agent_Found;
         return;
      end if;

      if Peer.Call = Call_Queue.null_Call then
         Yolk.Log.Trace (Yolk.Log.Debug,
                         "Hangup: The agent have no call to hangup");
         Status := No_Call_Found;
         return;
      end if;

      Yolk.Log.Trace (Yolk.Log.Debug,
                      "Call closed for " & To_String (Agent));

      AMI.Action.Action_Manager.Hangup
        (Ada.Strings.Unbounded.To_String (Peer.Call.Channel));

      Yolk.Log.Trace (Yolk.Log.Debug, "Hangup routine: after hangup Action");

      Peer.Call := Call_Queue.null_Call;
      Peers.Replace_Peer (Peer);
      Status := Success;

      Yolk.Log.Trace (Yolk.Log.Debug, "The Hangup routine is done.");
   end Hangup;

   --  TODO Refactor, in case something goes wrong,
   --    the steps are taken in the wrong order.
   --  Sends the agent's current call on hold / park.
   procedure Park (Agent  : in     String;
                   Call   :    out Call_Queue.Call_Type;
                   Status :    out Status_Type) is
      use Peers;
      use Peers.Peer_List_Type;
      use Call_Queue;
      --  Returns the format Asterisk understand "ChannelType/PhoneName"
--        function Get_PhoneInfo (Peer : in Peer_Type) return Unbounded_String;
--
--      function Get_PhoneInfo (Peer : in Peer_Type) return Unbounded_String is
--        begin
--           return Peer.ChannelType & To_Unbounded_String ("/") & Peer.Peer;
--        end Get_PhoneInfo;

      Peer : Peer_Type;
   begin
      --  Default fallback status.
      Status := Unknowen_Error;

      --  Finds the Agent, to get the call to park.
      Peer := Peers.Get_Peer (Peer => To_Unbounded_String (Agent));
      if Peer = Peers.null_Peer then
         --  No peer found
         Status := No_Agent_Found;
         Yolk.Log.Trace (Yolk.Log.Debug,
                         "Park Routine: The agent does not exsist: " & Agent);
         return;
      end if;

      if Peer.Call = Call_Queue.null_Call then
         Yolk.Log.Trace (Handle => Yolk.Log.Debug,
                         Message => "This agent have no call: " & Agent);
         Status := No_Call_Found;
         return;
      end if;

      Yolk.Log.Trace (Yolk.Log.Debug, "Park: Peer => " &
                        To_String (Peer.Peer) &
                        " Call Channel =>" & To_String (Peer.Call.Channel));
      --  Out parameter.
      Call := Peer.Call;

      --  Move the call back to the Queue, which will act like a parking lot.
      declare
         Exten : Unbounded_String;
      begin
         --  TODO Update, There have to be an easier way
         --   to find the companies Extension.
         --  Get the extension the queue is tied up to.

         AMI.Action.Action_Manager.Get_Var
           (Channel      => To_String (Call.Channel),
            VariableName => "Extension",
            Value        => Exten);

         if To_String (Exten) = "(null)" then
            Yolk.Log.Trace (Yolk.Log.Debug,
                            "Routines.Park: " &
                              "The Call does not have an Extension Variable" &
                              "Call-Channel: " & To_String (Call.Channel) &
                              "Peer: " & To_String (Peer.Peer));
            Status := Routines.Unknowen_Error;
            return;
         elsif To_String (Exten) = "" then
            Yolk.Log.Trace (Yolk.Log.Debug,
                            "Routines.Park: " &
                              "The Call have an empty extension variable" &
                              "Call-Channel: " & To_String (Call.Channel) &
                              "Peer: " & To_String (Peer.Peer));
            Status := Routines.Unknowen_Error;
            return;
         end if;

         --  Sets the variable that tells this call is a call on hold.
         AMI.Action.Action_Manager.Set_Var
           (Channel      => To_String (Call.Channel),
            VariableName => "CallState",
            Value        => "onhold");

         --  Move the call back to the queue
         AMI.Action.Action_Manager.Redirect
           (Channel => To_String (Call.Channel),
            Exten   => To_String (Exten),
            Context => "LocalSets");
      end;

      --  Set Variable that it's on hold
      --  Add it to the list
      --  //Make sure it is not added twice.

      --  Update peer information
      Peer.Parked_Calls.Append (Peer.Call);
      Peer.Call := Call_Queue.null_Call;
      Peers.Replace_Peer (Peer);

      Status := Success;
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
         Peer.ChannelType := To_Unbounded_String ("SIP");
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
   end TEST_StatusPrint;

   procedure UnPark (Agent    : in     String;
                     Call_ID  : in     String;
                     Status   :    out Status_Type) is
      use Peers;
      use Call_Queue;
      use Peers.Call_List;
      Peer : Peers.Peer_Type;
      Peer_Cursor : Cursor;
   begin
      Status := Unknowen_Error;
      --  First find the peer
      Peer := Peers.Get_Peer (To_Unbounded_String (Agent));
      if Peer = null_Peer then
         Status := No_Agent_Found;
         return;
      end if;

      if Peer.Call /= Call_Queue.null_Call then
         --  Something is wrong here.
         Yolk.Log.Trace
           (Yolk.Log.Info,
            "There were a request for unPark a call with Agent: " &
              Agent & " Call_ID: " & Call_ID &
              " Call allready there: " &
              To_String (Peer.Call.Channel));
         Status := Agent_Already_In_Call;
         return;
      end if;

      Yolk.Log.Trace (Yolk.Log.Debug,
                      "Routines.Unpark:" &
                        " Checkpoint before looking for the call");

      --  then find the call.
      Peer_Cursor := Peer.Parked_Calls.First;
      Find_Parked_Call :
      loop
         exit Find_Parked_Call when
           Peer_Cursor = Peers.Call_List.No_Element;

         --  when found, move it to current call.
         if Call_ID = Element (Peer_Cursor).Uniqueid then

            --  Move call from parked calls, to Current_call
            Peer.Call := Element (Peer_Cursor);
            Delete (Container => Peer.Parked_Calls,
                    Position  => Peer_Cursor);

            --  Make the actual Asterisk call to transfer the call.
            AMI.Action.Action_Manager.Redirect
              (Channel  => To_String (Peer.Call.Channel),
               Context  => "LocalSets",
               Exten    => To_String (Peer.Exten));

            --  Set the Variable CallState to empty,
            --    so we can see that the call is not on hold any more
            AMI.Action.Action_Manager.Set_Var
              (Channel      => To_String (Peer.Call.Channel),
               VariableName => "CallState",
               Value        => "");

            Status := Success;
            return;
         else
            Yolk.Log.Trace (Yolk.Log.Debug,
                            "Routines.Unpark: Call_ID /= Found id " &
                              Call_ID & " /= " & To_String
                              (Element (Peer_Cursor).Uniqueid));
         end if;
            Peer_Cursor := Next (Peer_Cursor);
      end loop Find_Parked_Call;

      Yolk.Log.Trace (Yolk.Log.Debug, "Routines.Unpark:" &
                        " Checkpoint after looking for parkedcall");
      --  If the call, for some unknown reason, wasn't there.
      Status := No_Call_Found;
   end UnPark;
end Routines;
