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

with System_Messages;

package body Model.Call is
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
      Call_List.Insert (Call);
   end Insert;

   function Get (Call_ID : in Call_ID_Type) return Call_Type is
   begin
      return Call_List.Get (Call_ID);
   end Get;

   --  Removes a specific call from the call queue.
   procedure Remove (Call_ID : Call_ID_Type) is
   begin
      Call_List.Remove (Call_ID);
   end Remove;

   procedure Update (Call : in Call_Type) is
   begin
      Call_List.Update (Call);
   end Update;

   function Dequeue (Call_ID : in Call_ID_Type) return Call_Type is
      Call : constant Call_Type := Call_List.Get (Call_ID);
   begin
      if Call = Null_Call then
         raise CALL_NOT_FOUND;
      else
         Call_List.Remove (Call_ID);
      end if;

      return Call;
   end Dequeue;

   -- -------------------- --
   -- Conversion Utilities --
   -- -------------------- --

   --  Gives the length of the call queue.
   function Length return Long_Integer is
   begin
      return Call_List.Length;
   end Length;

   function To_JSON (Call : in Call_Type) return JSON_Value is
      Root : constant JSON_Value := Create_Object;
      Value : constant JSON_Value := Create_Object;
      Org_ID : Ada.Strings.Unbounded.Unbounded_String;
      Org_Prefix : constant String := "org_id";
   begin
      if Call /= Null_Call then
         Org_ID := Ada.Strings.Unbounded.Tail
           (Call.Queue,
            Ada.Strings.Unbounded.Length (Call.Queue) - Org_Prefix'Length);

         Value.Set_Field ("channel", Call.Channel);
         Value.Set_Field ("caller_id", Call.CallerIDNum);
         Value.Set_Field ("org_id", Org_ID);
         Value.Set_Field ("call_id",  To_String (Call.ID));
         Value.Set_Field ("arrival_time", Unix_Timestamp (Call.Arrived));
         Root.Set_Field ("call", Value);
      end if;
      return Root;
   end To_JSON;

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

   -- -------------------- --
   -- Overloaded operators --
   -- -------------------- --

   function "=" (Left  : in Call_Type;
                 Right : in Call_Type) return Boolean is
   begin
      return (Left.ID.Timestamp = Right.ID.Timestamp) and
                (Left.ID.Sequence = Right.ID.Sequence);
   end  "=";

end Model.Call;
