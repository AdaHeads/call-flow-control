-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                Model.Calls                                --
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

--  Declares a call list with corresponding primitives.
--  Currently also holds a singleton call list object for easy access.

with Ada.Strings.Unbounded;
with System_Messages;

package body Model.Calls is
   use System_Messages;

   protected body Protected_Call_List_Type is
      function Contains (Call_ID : in Call_ID_Type) return Boolean is
      begin
         return List.Contains (Call_ID);
      end Contains;

      --  Places the call, in the queue.
      procedure Insert (Call : in Call_Type) is
      begin
         List.Insert
           (Key       => Call.ID,
            New_Item  => Call);

      exception
         when Constraint_Error =>
            if List.Element (Key => Call.ID).ID = Call.ID then
               raise DUPLICATE_ID with "key " & To_String (Call.ID) &
                 " already in map.";
            end if;
            raise;
      end Insert;

      --  Returns the entire queue.
      procedure For_Each (Process : in Call_Process_Type) is
      begin
         for I of List loop
            Process (I);
         end loop;
      end For_Each;

      function Get (Call_ID : in Call_ID_Type) return Call_Type is
         Call : constant Call_Type :=
           List.Element (Key => Call_ID);
      begin
         if Call = Null_Call then
            raise CALL_NOT_FOUND;
         end if;

         return Call;
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

      function To_JSON return JSON_Value is
         Value     : JSON_Value := Create_Object;
         JSON_List : JSON_Array;
         Root      : constant JSON_Value := Create_Object;
      begin
         for Call of List loop
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
         for Call of List loop
            Append (Item, To_String (Call));
         end loop;
         return To_String (Item);
      end To_String;

      procedure Update (Call : in Call_Type) is
      begin
         if List.Contains (Call.ID) then
            List.Replace (Key      => Call.ID,
                          New_Item => Call);
         else
            raise CALL_NOT_FOUND;
         end if;
      end Update;

      function Next return Call_Type is
      begin
         return List.First_Element;
      end Next;
   end Protected_Call_List_Type;

   -- -------- --
   -- Wrappers --
   -- -------- --

   --  Places a call on the call queue.
   procedure Insert (Call : in Call_Type) is
   begin
      List.Insert (Call);
   end Insert;

   function Get (Call_ID : in Call_ID_Type) return Call_Type is
   begin
      return List.Get (Call_ID);
   end Get;

   --  Removes a specific call from the call queue.
   procedure Remove (Call_ID : Call_ID_Type) is
   begin
      List.Remove (Call_ID);
   end Remove;

   procedure Update (Call : in Call_Type) is
   begin
      List.Update (Call);
   end Update;

   function Dequeue (Call_ID : in Call_ID_Type) return Call_Type is
      Call : constant Call_Type := List.Get (Call_ID);
   begin
      if Call = Null_Call then
         raise CALL_NOT_FOUND;
      else
         List.Remove (Call_ID);
      end if;

      return Call;
   end Dequeue;

   -- -------------------- --
   -- Conversion Utilities --
   -- -------------------- --

   --  Gives the length of the call queue.
   function Length return Long_Integer is
   begin
      return List.Length;
   end Length;
end Model.Calls;
