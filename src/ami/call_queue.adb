with Ada.Characters.Latin_1,
     Yolk.Log;

package body Call_Queue is
   use Queue_Type;

   protected Call_Queue is
      procedure Enqueue (Call : in Call_Type);
      --  Places a call on the callqueue.

      procedure Dequeue (Call : out Call_Type);
      --  Takes the next call in the queue.

      procedure Dequeue (Uniqueid : in     Unbounded_String;
                         Call     :    out Call_Type);
      --  Takes out a specific call from the callqueue.

      procedure Remove  (Uniqueid : in Unbounded_String);
      --  Removes a specific call.

      function Get_Queue      return Call_Queue_Type;
      --  Returns the entire queue.

      function Queue_Length   return Ada.Containers.Count_Type;
      --  Gives the number of calls waiting in the call queue.

      function Queue_ToString return Unbounded_String;
      --  Return a Debug friendly String representation of the callqueue.
   private
      Queue : Call_Queue_Type;
   end Call_Queue;

   protected body Call_Queue is
      --  Gives the call with the highest priority, and have waited longest.
      procedure Dequeue (Call : out Call_Type) is
      begin
         Call := null_Call;
         for i in Queue'Range loop
            if not Queue_Type.Is_Empty (Queue (i)) then
               Call := Queue_Type.First_Element (Queue (i));
               Queue_Type.Delete_First (Container => Queue (i));
            end if;
         end loop;
      end Dequeue;

      --  Gives the call with selected Uniqueid
      procedure Dequeue (Uniqueid : in     Unbounded_String;
                         Call     :    out Call_Type) is
         Call_Queue_Cursor : Queue_Type.Cursor;
         Current_Call : Call_Type;
      begin
         for i in Queue'Range loop
            Call_Queue_Cursor := Queue_Type.First (Container => Queue (i));
            loop
               exit when Call_Queue_Cursor = Queue_Type.No_Element;
               Current_Call := Queue_Type.Element (Call_Queue_Cursor);
               if Current_Call.Uniqueid = Uniqueid then
                  Call := Current_Call;
                  Queue_Type.Delete (Container => Queue (i),
                                     Position  => Call_Queue_Cursor,
                                     Count     => 1);
                  return;
               end if;
            end loop;

            if not Queue_Type.Is_Empty (Queue (i)) then
               Call := Queue_Type.First_Element (Queue (i));
               Queue_Type.Delete_First (Container => Queue (i));
            end if;
         end loop;
         Call := null_Call;
      end Dequeue;

      --  Places the call, in the queue.
      procedure Enqueue (Call : in Call_Type) is
         Company_Priority : constant Priority_Level :=
           Get_Company_Priority (Call.Queue);
      begin
         Queue_Type.Append (Container => Queue (Company_Priority),
                            New_Item => Call);
      end Enqueue;

      --  Returns the entire queue.
      function Get_Queue return Call_Queue_Type is
      begin
         return Queue;
      end Get_Queue;

      --  Returns the number of calls waiting in a queue.
      function Queue_Length return Ada.Containers.Count_Type is
         use Ada.Containers;

         Total_Calls : Count_Type := 0;
      begin
         for Queue_Priority in Queue'Range loop
            Total_Calls := Total_Calls + Queue (Queue_Priority).Length;
         end loop;
         return Total_Calls;
      end Queue_Length;

      --  Returns the Call queues, as text.
      function Queue_ToString return Unbounded_String is
         text : Unbounded_String;
         package Char renames Ada.Characters.Latin_1;
      begin
         Append (Source => text,
                 New_Item => To_Unbounded_String ("--==Call Queue==--" &
                     Char.LF));

         for i in Queue'Range loop
            Append (text,
              To_Unbounded_String ("Queue" &
                  Priority_Level'Image (i) & Char.LF));
            --  Ada 2012
            for Queue_Index in Queue (i).Iterate loop
               Append (text, Queue_Type.Element (Queue_Index).Channel);
               Append (text, ", Uniqueid: ");
               Append (text, Queue_Type.Element (Queue_Index).Uniqueid);
               Append (text, Char.LF);
            end loop;
         end loop;
         return text;
      end Queue_ToString;

      --  Removes the call with the specified UniqueID
      procedure Remove (Uniqueid : in Unbounded_String) is
         Call_Queue_Element : Call_Type;
      begin
         for i in Queue'Range loop
            for Call_Queue_Index in
              Queue (i).First_Index .. Queue (i).Last_Index loop
               Call_Queue_Element := Queue (i).Element (Call_Queue_Index);
               if Call_Queue_Element.Uniqueid = Uniqueid then
                  Queue (i).Delete (Call_Queue_Index);
                  return;
                  --  we have found and deleted the call, now, exit procedure.
               end if;
            end loop;
         end loop;

         Yolk.Log.Trace
           (Yolk.Log.Debug,
            "Remove: This uniqueid could not be found in the call queue." &
              " Uniqueid: " & To_String (Uniqueid));
      end Remove;
   end Call_Queue;

      --  Returns a call in String format.
   function Call_To_String (Call : in Call_Type) return String is
      Response : Unbounded_String;
   begin
      Append (Response, "Channel => " & To_String (Call.Channel));
      Append (Response, ", Queue => " & To_String (Call.Queue));
      Append (Response, ", Uniqueid => " & To_String (Call.Uniqueid));
      return To_String (Response);
   end Call_To_String;

   --  Takes the first call with the highest priority
   procedure Dequeue (Call : out Call_Type) is
   begin
      Call_Queue.Dequeue (Call);
   end Dequeue;

   --  Takes a specific call out from the call queue.
   procedure Dequeue (Uniqueid : in Unbounded_String; Call : out Call_Type) is
   begin
      Call_Queue.Dequeue (Uniqueid, Call);
   end Dequeue;

   --  Places a call on the call queue.
   procedure Enqueue (Call : in Call_Type) is
   begin
      Call_Queue.Enqueue (Call);
   end Enqueue;

   --  Returns the priority of the call, based on the company
   function Get_Company_Priority (CompanyName : Unbounded_String)
                                  return Priority_Level is
   begin
      if To_String (CompanyName) = "org_id1" then
         return High;
      elsif To_String (CompanyName) = "org_id2" then
         return Normal;
      else
         return Low;
      end if;
   end Get_Company_Priority;

   --  Returns the entire call queue.
   function Get_Queue return Call_Queue_Type is
   begin
      return Call_Queue.Get_Queue;
   end Get_Queue;

   --  Gives the length of the call queue.
   function Queue_Length return Ada.Containers.Count_Type is
   begin
      return Call_Queue.Queue_Length;
   end Queue_Length;

   --  Returns a debug friendly String representation of the call queue.
   function Queue_ToString return Unbounded_String is
   begin
      return Call_Queue.Queue_ToString;
   end Queue_ToString;

   --  Removes a specific call from the call queue.
   procedure Remove (Uniqueid : in Unbounded_String) is
   begin
      Call_Queue.Remove (Uniqueid);
   end Remove;
end Call_Queue;
