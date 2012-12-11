-------------------------------------------------------------------------------
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

--  Declares a call list with corresponding primitives.
--  Currently also holds a singleton call list object for easy access.

with Ada.Strings.Unbounded;
with Model.Agent_ID;

package body Model.Calls is

   protected body Protected_Call_List_Type is
      procedure Assign (Agent : in     Agent_Type;
                        Call  : in out Call_Type) is
         use Call_Storage;
         use Model.Agent_ID;

         C : constant Cursor :=
               Protected_List.Find (Call.ID);
      begin
         if C /= Call_Storage.No_Element then
            Call := Element (Position => C);

            if Call.Assigned_To /= Null_Agent_ID then
               Call.Assigned_To := Agent.ID;
               Protected_List.Replace_Element (C, Call);
            else
               raise Already_Assigned;
            end if;
         else
            Call := Null_Call;
         end if;
      end Assign;

      function Contains (Call_ID : in Call_ID_Type) return Boolean is
      begin
         return Protected_List.Contains (Call_ID);
      end Contains;

      -------------
      -- Dequeue --
      -------------

      procedure Dequeue
        (Call_ID : in     Call_ID_Type := Null_Call_ID;
         Call    : in out Call_Type) is
      begin
         Call := Null_Call;

         --  If no call ID is supplied we, by default, pick up the
         --  first element
         if Call_ID = Null_Call_ID then
            if not Protected_List.Is_Empty then
               Call := Protected_List.First_Element;
            end if;
         else
            if Protected_List.Contains (Key => Call_ID) then
               Call := Protected_List.Element (Call_ID);
            end if;
         end if;
      end Dequeue;

      --  Process the entire queue.
      procedure For_Each (Process : in Call_Process_Type) is
      begin
         for I of Protected_List loop
            Process (I);
         end loop;
      end For_Each;

      function Get (Call_ID : in Call_ID_Type) return Call_Type is
         Call : constant Call_Type :=
           Protected_List.Element (Key => Call_ID);
      begin
         if Call = Null_Call then
            raise Call_Not_Found;
         end if;

         return Call;
      end Get;

      --  Places the call, in the queue.
      procedure Insert (Call : in Call_Type) is
      begin
         Protected_List.Insert
           (Key       => Call.ID,
            New_Item  => Call);

      exception
         when Constraint_Error =>
            if Protected_List.Element (Key => Call.ID).ID = Call.ID then
               raise Duplicate_ID with "key " & To_String (Call.ID) &
                 " already in map.";
            end if;
            raise;
      end Insert;

      function Is_Empty return Boolean is
      begin
         return Protected_List.Is_Empty;
      end Is_Empty;

      --  Returns the total number of calls.
      function Length return Natural is
         use Ada.Containers;
      begin
         return Natural (Protected_List.Length);
      end Length;

      procedure Put (Call : in Call_Type) is
      begin
         raise Program_Error;
      end Put;

      --  Removes the call with the specified UniqueID
      procedure Remove (Call_ID : Call_ID_Type) is
      begin
         Protected_List.Delete (Call_ID);
      end Remove;

      function To_JSON return JSON_Value is
         Value     : JSON_Value := Create_Object;
         JSON_List : JSON_Array;
         Root      : constant JSON_Value := Create_Object;
      begin
         for Call of Protected_List loop
            if Call /= Null_Call then
               Value := Call.To_JSON;
               Append (JSON_List, Value);
            end if;
         end loop;
         Root.Set_Field ("calls", JSON_List);
         return Root;
      end To_JSON;

      function To_String return String is
         use Ada.Strings.Unbounded;
         Item : Unbounded_String;
      begin
         for Call of Protected_List loop
            Append (Item, Call.To_String);
         end loop;
         return To_String (Item);
      end To_String;

      procedure Update (Call : in Call_Type) is
      begin
         if Protected_List.Contains (Call.ID) then
            Protected_List.Replace (Key      => Call.ID,
                          New_Item => Call);
         else
            raise Call_Not_Found;
         end if;
      end Update;
   end Protected_Call_List_Type;
end Model.Calls;
