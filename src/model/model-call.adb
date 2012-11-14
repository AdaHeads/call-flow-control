-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                Call_List                                  --
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

with Ada.Strings.Fixed;

with System_Messages;

package body Model.Call is
   use System_Messages;
   
   protected body Protected_Call_List is
      --  Places the call, in the queue.
      procedure Insert (Call : in Call_Type) is
      begin
         List.Insert 
           (Key       => Call.ID,
            New_Item  => Call);
      end Insert;

      --  Returns the entire queue.
      function Get_List return Call_List_Type.Map is
      begin
         return List;
      end Get_List;

      function Get (Call_ID : in Call_ID_Type) return Call_Type is
      begin
         return Call_List_Type.Element (Container => List, 
                          Key       => Call_ID);
      end Get;

      --  Returns the total number of calls.
      function Length return Long_Integer is
         use Ada.Containers;
      begin
         return Long_Integer (List.Length);
      end Length;

      --  Removes the call with the specified UniqueID
      procedure Remove (Call_ID : Call_ID_Type) is
      begin
         List.Delete (Call_ID);
         System_Messages.Notify
           (Debug,
            "Call_List - Remove:" &
              "This uniqueid could not be found in the call queue." &
              " Call.ID: " & To_String (Call_ID));
      end Remove;

      procedure Update (Call : in Call_Type) is
      begin
         List.Replace (Key      => Call.ID,
                       New_Item => Call);
      end Update;

      function Next return Call_Type is
      begin
         return List.First_Element;
      end Next;
   end Protected_Call_List;

   --  Places a call on the call queue.
   procedure Insert (Call : in Call_Type) is
   begin
      Protected_Call_List.Insert (Call);
   end Insert;

   --  Returns the entire call queue.
   function Get_List return Call_List_Type.Map is
   begin
      return Protected_Call_List.Get_List;
   end Get_List;

   function Get (Call_ID : in Call_ID_Type) return Call_Type is
   begin
      return Protected_Call_List.Get (Call_ID);
   end Get;

   --  Returns a call in String format.
   function To_String (Call : in Call_Type) return String is
      Response : Unbounded_String;
   begin
      Append (Response, "ID => " & To_String (Call.ID));
      Append (Response, ", Channel => "    & To_String (Call.Channel));
      Append (Response, ", Queue => "    & To_String (Call.Queue));
      Append (Response, ", State => "    & Call.State'Img);

      return To_String (Response);
   end To_String;

   --   Returns a debug friendly String representation of the call queue.
   function Image return String is
   begin
      return "";--Protected_Call_List.;
   end Image;

     --  Gives the length of the call queue.
   function Length return Long_Integer is
   begin
      return Protected_Call_List.Length;
   end Length;

   function Dequeue (Call_ID : in Call_ID_Type) return Call_Type is
      Call : constant Call_Type := Protected_Call_List.Get (Call_ID);
   begin
      if Call /= Null_Call then
         Protected_Call_List.Remove (Call_ID);
      end if;
        return Call;
   end Dequeue;
   
   --  Removes a specific call from the call queue.
   procedure Remove (Call_ID : Call_ID_Type) is
   begin
      Protected_Call_List.Remove (Call_ID);
   end Remove;

   procedure Update (Call : in Call_Type) is
   begin
      Protected_Call_List.Update (Call);
   end Update;

   function Next return Call_Type is
   begin
      return Protected_Call_List.Next;
   end Next;
   
   function To_Call_ID (Item : String) return Call_ID_Type is
      Offset : constant Natural := Index ('.', Item);
   begin
      if Offset < 3 then 
         return Null_Call_ID;
      else
         return 
           (Timestamp => Integer'Value 
              (Item (Item'First .. Item'First+Offset-2)),
            Sequence => Integer'Value 
              (Item (Item'First+Offset .. Item'Last)));
      end if;
   end To_Call_ID;
   
   function To_String (Call_ID : in Call_ID_Type) return String is
   begin
      return Ada.Strings.Fixed.Trim
        (Integer'Image (Call_ID.Timestamp),
         Ada.Strings.Left) & 
        "." &
        Ada.Strings.Fixed.Trim
        (Integer'Image (Call_ID.Sequence),
         Ada.Strings.Left);
   end To_String;
   
   function "=" (Left  : in Call_Type;
                 Right : in Call_Type) return Boolean is
   begin
      return (Left.ID.Timestamp = Right.ID.Timestamp) and 
                (Left.ID.Sequence = Right.ID.Sequence);
   end  "=";
   
   function "<" (Left  : in Call_ID_Type;
                 Right : in Call_ID_Type) return Boolean is
   begin
      if Left.Timestamp = Right.Timestamp then
         return Left.Sequence < Right.Sequence;
      else
         return Left.Timestamp < Right.Timestamp;
      end if;
   end "<";
           
end Model.Call;


   --   --  Gives the call with the highest priority, and have waited longest.
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
      --      Call_Queue_Cursor := Queue_Type.First (Container => Queue (i));
      --              loop
      --                 exit when Call_Queue_Cursor = Queue_Type.No_Element;
      --               Current_Call := Queue_Type.Element (Call_Queue_Cursor);
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
   --     function PickupCall (Agent_ID : in Unbounded_String,
   --         Uniqueid : in Unbounded_String) return Call_Type is
   --        Call : Call_Type;
   --     begin
   --        Protected_Call_List.PickupCall (Agent_ID, Uniqueid, Call);
   --        return Call;
   --     end PickupCall;

   --     --  Takes the first call with the highest priority
   --     procedure Dequeue (Call : out Call_Type) is
   --     begin
   --        Call_Queue.Dequeue (Call);
   --     end Dequeue;

   --     --  Takes a specific call out from the call queue.
   --  procedure Dequeue
--       (Uniqueid : in Unbounded_String; Call : out Call_Type) is
   --     begin
   --        Call_Queue.Dequeue (Uniqueid, Call);
   --     end Dequeue;

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

   --     function Hangup
   --       (CallID : in Unbounded_String) return Call_Type is
   --        Call : Call_Type;
   --     begin
   --        Protected_Call_List.Hangup (CallID, Call);
   --        return Call;
   --     end Hangup;


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
      --                  "The Call with Uniqueid: " & To_String (Uniqueid) &
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


      --  --  Returns the call list as String.
      --  function Image return String is
      --     Text : Unbounded_String;
      --     package Char renames Ada.Characters.Latin_1;
      --  begin
      --     Append (Text, "Queue content: ");
      --     for Call in List.Iterate loop
      --        Append (Text, Image(Call_List_Type.Element (Call)));
      --        Append (Text, Char.LF);
      --     end loop;
      --     return To_String (Text);
      --  end Image;
