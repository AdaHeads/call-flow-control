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

      ------------
      -- Assign --
      ------------

      procedure Assign (Agent : in     Agent_Type;
                        Call  : in out Call_Type) is
         use Call_Storage;
         use Model.Agent_ID;

         C : Cursor := Call_Storage.No_Element;

      begin
         if Call = Null_Call then
            C := Protected_List.First;
         else
            C := Protected_List.Find (Call.ID);
         end if;

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

      --------------
      -- Contains --
      --------------

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

      --------------
      -- For_Each --
      --------------

      procedure For_Each (Process : in Call_Process_Type) is
      begin
         for I of Protected_List loop
            Process (I);
         end loop;
      end For_Each;

      ---------
      -- Get --
      ---------

      function Get (Call_ID : in Call_ID_Type) return Call_Type is
         Call : Call_Type := Null_Call;
      begin
         if Call_ID /= Null_Call_ID then
            Call := Protected_List.Element (Key => Call_ID);
         end if;

         if Call = Null_Call then
            raise Call_Not_Found;
         end if;

         return Call;
      end Get;

      ------------
      -- Insert --
      ------------

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

      --------------
      -- Is_Empty --
      --------------

      function Is_Empty return Boolean is
      begin
         return Protected_List.Is_Empty;
      end Is_Empty;

      -------------
      -- Length --
      -------------

      function Length return Natural is
         use Ada.Containers;
      begin
         return Natural (Protected_List.Length);
      end Length;

      ----------
      -- Link --
      ----------

      procedure Link (ID1 : Call_ID.Call_ID_Type;
                      ID2 : Call_ID.Call_ID_Type) is
         Call1 : Call.Call_Type := Call.Null_Call;
         Call2 : Call.Call_Type := Call.Null_Call;
      begin
         --  Sanity checks
         if not Protected_List.Contains (ID1) then
            raise Call_Not_Found with ID1.To_String;
         elsif not Protected_List.Contains (ID2) then
            raise Call_Not_Found with ID2.To_String;
         end if;

         Call1 := Protected_List.Element (ID1);
         Call2 := Protected_List.Element (ID2);

         Call1.State := Bridged;
         Call2.State := Bridged;

         Call1.Bridged_With := ID2;
         Call2.Bridged_With := ID1;

         Protected_List.Replace (ID1, Call1);
         Protected_List.Replace (ID2, Call2);
      end Link;

      ---------
      -- Put --
      ---------

      procedure Put (Call : in Call_Type) is
      begin
         if not Protected_List.Contains (Call.ID) then
            Protected_List.Insert (Call.ID, Call);
         else
            Protected_List.Replace (Call.ID, Call);
         end if;
      end Put;

      ------------
      -- Remove --
      ------------

      procedure Remove (Call_ID : Call_ID_Type) is
      begin
         Protected_List.Delete (Call_ID);
      end Remove;

      -------------
      -- To_JSON --
      -------------

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

      ---------------
      -- To_String --
      ---------------

      function To_String return String is
         use Ada.Strings.Unbounded;
         Item : Unbounded_String;
      begin
         for Call of Protected_List loop
            Append (Item, Call.To_String);
         end loop;
         return To_String (Item);
      end To_String;

      ------------
      -- Unlink --
      ------------

      procedure Unlink (ID1 : Call_ID.Call_ID_Type;
                        ID2 : Call_ID.Call_ID_Type) is
         Call1 : Call.Call_Type := Call.Null_Call;
         Call2 : Call.Call_Type := Call.Null_Call;
      begin
         --  Sanity checks
         if not Protected_List.Contains (ID1) then
            raise Call_Not_Found with ID1.To_String;
         elsif not Protected_List.Contains (ID2) then
            raise Call_Not_Found with ID2.To_String;
         end if;

         Call1 := Protected_List.Element (ID1);
         Call2 := Protected_List.Element (ID2);

         --  This should only be unknown for a very brief period.
         Call1.State := Unknown;
         Call2.State := Unknown;

         Call1.Bridged_With := Null_Call_ID;
         Call2.Bridged_With := Null_Call_ID;

         Protected_List.Replace (ID1, Call1);
         Protected_List.Replace (ID2, Call2);
      end Unlink;

      ------------
      -- Update --
      ------------

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
