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

with Ada.Containers.Ordered_Maps;
with Model.Agent;
with Model.Call;
with Model.Call_ID;
with GNATCOLL.JSON;

package Model.Calls is
   use GNATCOLL.JSON;
   use Model;
   use Model.Agent;
   use Model.Call_ID;
   use Model.Call;

   Call_Not_Found   : exception;
   Duplicate_ID     : exception;
   Null_Exception   : exception;
   Already_Assigned : exception;

   type Call_Process_Type is not null access procedure (Call : in Call_Type);

   package Call_Storage is new
     Ada.Containers.Ordered_Maps (Key_Type   => Call_ID_Type,
                                  Element_Type => Call_Type);

   protected type Protected_Call_List_Type is
      procedure Assign (Agent  :  in     Agent_Type;
                        Call   :  in out Call_Type);
      --  Assign a given call to an agent. This call merely update the
      --  Links between the two recordt types.

      function Contains (Call_ID : in Call_ID_Type) return Boolean;
      --  Detemine if a call is already represented in the call queue.

      procedure Link (ID1 : Call_ID.Call_ID_Type;
                      ID2 : Call_ID.Call_ID_Type);
      --  Links two calls, marking them as interconnected.

      procedure Unlink (ID1 : Call_ID.Call_ID_Type;
                      ID2 : Call_ID.Call_ID_Type);
      --  Unlinks two calls, marking them as disconnected.

      procedure Dequeue
        (Call_ID : in     Call_ID_Type := Null_Call_ID;
         Call    : in out Call_Type);
      --  Removes the call with the specified Call_ID and returns it in Call
      --  If no Call_ID (or a Null_Call_ID) is supplied, the call will
      --  Dequeue the first element from the list.

      function Get (Call_ID : in Call_ID_Type) return Call_Type;
      --  Returns the call with the specified Call_ID, or Null_Call if the
      --  Call_ID is not present.

      function Is_Empty return Boolean;
      --  Returns true if the list contains no calls.

      procedure Insert (Call : in Call_Type);
      --  Places a call in the call_List or raises Duplicate_ID if the call is
      --  already in the queue.

      procedure Put (Call : in Call_Type);
      --  Inserts or updates a call regardless of whether the call exists in
      --  the list or not.

      procedure Remove (Call_ID : in Call_ID_Type);
      --  Removes a specific call from the call queue.

      function Length return Natural;
      --  Returns the length of the queue.

      function To_JSON return JSON_Value;
      --  Convert the entire queue to a JSON array

      function To_String return String;
      --  Debug-friendly represenation of the list.

      procedure For_Each (Process : in Call_Process_Type);
      --  Primitive iterator.

      procedure Update (Call : in Call_Type);
      --  Replaces the call at the ID location with the call supplied.
      --  Raises Call_Not_Found if there is no call to replace.

   private
      Protected_List : Call_Storage.Map;
   end Protected_Call_List_Type;

   List : Protected_Call_List_Type;
   --  Package-visible singleton.
end Model.Calls;
