with Ada.Characters.Latin_1,
     Yolk.Log;

package body Call_List is
   use Queue_Type;

   protected Protected_Call_List is
      procedure Add (Call : in Call_Type);
      --  Places a call on the callqueue.

--        procedure Dequeue (Call : out Call_Type);
--        --  Takes the next call in the queue.
--
--        procedure Dequeue (Uniqueid : in     Unbounded_String;
--                           Call     :    out Call_Type);
--        --  Takes out a specific call from the callqueue.

      function Remove (Uniqueid : in Unbounded_String) return Call_Type;
      --  Removes a specific call.

      function Get return Call_Queue_Type;
      --  Returns the entire queue.

      function Get_Call (UniqueID : in Unbounded_String) return Call_Type;
      --  Returns the call with that UniqueID.

      function Length return Ada.Containers.Count_Type;
      --  Gives the number of calls waiting in the call queue.

      function ToString return Unbounded_String;
      --  Return a Debug friendly String representation of the list.

--        procedure PickupCall (Agent_ID : in Unbounded_String,
--                              Uniqueid : in Unbounded_String,
--                              Call     : out Call_Type);

      procedure Update (Call : in Call_Type);
   private
      List : Call_List_Type;
   end Protected_Call_List;

   protected body Protected_Call_List is
--        --  Gives the call with the highest priority, and have waited longest.
--        procedure Dequeue (Call : out Call_Type) is
--        begin
--           Call := null_Call;
--           for i in Queue'Range loop
--              if not Queue_Type.Is_Empty (Queue (i)) then
--                 Call := Queue_Type.First_Element (Queue (i));
--                 Queue_Type.Delete_First (Container => Queue (i));
--              end if;
--           end loop;
--        end Dequeue;

--        --  Gives the call with selected Uniqueid
--        procedure Dequeue (Uniqueid : in     Unbounded_String;
--                           Call     :    out Call_Type) is
--           Call_Queue_Cursor : Queue_Type.Cursor;
--           Current_Call : Call_Type;
--        begin
--           for i in Queue'Range loop
--              Call_Queue_Cursor := Queue_Type.First (Container => Queue (i));
--              loop
--                 exit when Call_Queue_Cursor = Queue_Type.No_Element;
--                 Current_Call := Queue_Type.Element (Call_Queue_Cursor);
--                 if Current_Call.Uniqueid = Uniqueid then
--                    Call := Current_Call;
--                    Queue_Type.Delete (Container => Queue (i),
--                                       Position  => Call_Queue_Cursor,
--                                       Count     => 1);
--                    return;
--                 end if;
--              end loop;
--
--              if not Queue_Type.Is_Empty (Queue (i)) then
--                 Call := Queue_Type.First_Element (Queue (i));
--                 Queue_Type.Delete_First (Container => Queue (i));
--              end if;
--           end loop;
--           Call := null_Call;
--        end Dequeue;

      --  Places the call, in the queue.
      procedure Add (Call : in Call_Type) is
--           Company_Priority : constant Priority_Level :=
--             Get_Company_Priority (Call.Queue);
      begin
         List.Append (Call);
--           Queue_Type.Append (Container => Queue (Company_Priority),
--                              New_Item  => Call);
      end Add;

      --  Returns the entire queue.
      function Get return Call_List_Type.Vector is
      begin
         return List;
      end Get;

      function Get_Call (UniqueID : in Unbounded_String) return Call_Type is
      begin
         for Call in List loop
            if Call.UniqueID = UniqueID then
               return Call;
            end if;
         end loop;
         return null_Call;
      end Get_Call;

      --  Returns the total number of calls.
      function Length return Ada.Containers.Count_Type is
         use Ada.Containers;
      begin
         return List.Length();
      end Length;

      --  Returns the call list as String.
      function ToString return Unbounded_String is
         text : Unbounded_String;
         package Char renames Ada.Characters.Latin_1;
      begin
         for Call in List loop
            Append (text, Call.Channel);
            Append (text, ", Uniqueid: ");
            Append (text, Call.Uniqueid);
            Append (text, Char.LF);
         end Loop;
         return text;
      end ToString;

      --  Removes the call with the specified UniqueID
      function Remove (Uniqueid : in Unbounded_String) return Call_Type is
         Call_Queue_Element : Call_Type;
      begin
         for index in List.First_Index .. List.Last_Index loop
            if List.Element (Position => index).Uniqueid = Uniqueid then
               return List.Delete (Position => Index);
               --  we have found and deleted the call, now, exit procedure.
            end if;
         end loop;

--           for i in Queue'Range loop
--              for Call_Queue_Index in
--                Queue (i).First_Index .. Queue (i).Last_Index loop
--                 Call_Queue_Element := Queue (i).Element (Call_Queue_Index);
--                 if Call_Queue_Element.Uniqueid = Uniqueid then
--                    Queue (i).Delete (Call_Queue_Index);
--                    return;
--                    --  we have found and deleted the call, now, exit procedure.
--                 end if;
--              end loop;
--           end loop;
         Yolk.Log.Trace
           (Yolk.Log.Debug,
            "Call_List - Remove:" &
              "This uniqueid could not be found in the call queue." &
              " Uniqueid: " & To_String (Uniqueid));
         return null_Call;
      end Remove;

--        procedure PickupCall (Agent_ID : in Unbounded_String,
--                              Uniqueid : in Unbounded_String
--                              Call     : out Call_Type) is
--           Temp_Call : Call_Type;
--        begin
--           --  First find a call
--           if Uniqueid = Null_Unbounded_String then
--              for item in List loop
--                 if item.State = Queued then
--                    Temp_Call := item;
--                 end if;
--              end loop;
--           else
--              for item in List loop
--                 if item.Uniqueid = Uniqueid then
--                    if item.State /= Queued then
--                       Yolk.Log.Trace
--                         (Yolk.Log.Debug,
--                          "Call_List - PickupCall:" &
--                            "The Call with Uniqueid: " & To_String (Uniqueid) &
--                         " is in state: " item.State'Img);
--                    else
--                       Temp_Call := item;
--                    end if;
--                 end if;
--              end loop;
--           end if;
--
--           --  Second insert Agent_ID and change some fields.
--           Call.Agent_ID = Agent_ID;
--           Call.State = Speaking;
--           Call.Is_Picked_Up = True;
--           Call.Picked_Up = Ada.Calendar.Time;
--
--           --  Thrid update Call element.
--           Update (Temp_Call);
--
--           --  fourth return the call
--           Call := Temp_Call;
--        end PickupCall;

      procedure Update (Call : in Call_Type) is
      begin
         --  find waldos code.
         raise Program_Error;
      end Update;
   end Protected_Call_List;

      --  Returns a call in String format.
   function Call_To_String (Call : in Call_Type) return String is
      Response : Unbounded_String;
   begin
      Append (Response, "Channel => " & To_String (Call.Channel));
      Append (Response, ", Queue => " & To_String (Call.Queue));
      Append (Response, ", Uniqueid => " & To_String (Call.Uniqueid));
      return To_String (Response);
   end Call_To_String;

--     --  Takes the first call with the highest priority
--     procedure Dequeue (Call : out Call_Type) is
--     begin
--        Call_Queue.Dequeue (Call);
--     end Dequeue;

--     --  Takes a specific call out from the call queue.
--     procedure Dequeue (Uniqueid : in Unbounded_String; Call : out Call_Type) is
--     begin
--        Call_Queue.Dequeue (Uniqueid, Call);
--     end Dequeue;

   --  Places a call on the call queue.
   procedure Add (Call : in Call_Type) is
   begin
      Protected_Call_List.Add (Call);
   end Add;

--     --  Returns the priority of the call, based on the company
--     function Get_Company_Priority (CompanyName : Unbounded_String)
--                                    return Priority_Level is
--     begin
--        if To_String (CompanyName) = "org_id1" then
--           return High;
--        elsif To_String (CompanyName) = "org_id2" then
--           return Normal;
--        else
--           return Low;
--        end if;
--     end Get_Company_Priority;

   --  Returns the entire call queue.
   function Get return Call_Queue_Type is
   begin
      return Protected_Call_List.Get;
   end Get;

   --  Gives the length of the call queue.
   function Length return Ada.Containers.Count_Type is
   begin
      return Protected_Call_List.Length;
   end Length;

   --  Returns a debug friendly String representation of the call queue.
   function ToString return Unbounded_String is
   begin
      return Protected_Call_List.ToString;
   end ToString;

   --  Removes a specific call from the call queue.
   function Remove (Uniqueid : in Unbounded_String) return Call_Type is
   begin
      return Protected_Call_List.Remove (Uniqueid);
   end Remove;

--     function PickupCall (Agent_ID : in Unbounded_String,
--                          Uniqueid : in Unbounded_String) return Call_Type is
--        Call : Call_Type;
--     begin
--        Protected_Call_List.PickupCall (Agent_ID, Uniqueid, Call);
--        return Call;
--     end PickupCall;

   procedure Update (Call : in Call_Type) is
   begin
      Protected_Call_List.Update (Call);
   end Update;

--     function Hangup
--       (CallID : in Unbounded_String) return Call_Type is
--        Call : Call_Type;
--     begin
--        Protected_Call_List.Hangup (CallID, Call);
--        return Call;
--     end Hangup;

   function Get_Call (UniqueID : in Unbounded_String) return Call_Type is
   begin
      return Protected_Call_List.Get_Call (UniqueID);
   end Get_Call;
end Call_List;
