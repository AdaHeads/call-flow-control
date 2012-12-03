-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                Model.Calls                                --
--                                                                           --
--                                  SPEC                                     --
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
with Model.Call;
with Model.Call_ID;
with GNATCOLL.JSON;

package Model.Calls is
   use GNATCOLL.JSON;
   use Model.Call_ID;
   use Model.Call;

   CALL_NOT_FOUND  : exception;
   DUPLICATE_ID    : exception;

   procedure Insert (Call : in Call_Type);
   --  Places a call in the call_List.

   procedure Remove (Call_ID : in Call_ID_Type);
   --  Removes a specific call from the call queue.

   procedure Update (Call : in Call_Type);
   --  Updates a Call in the list, with the information from the Call Argument

   function Length return Long_Integer;
   --  Gives the length of the call queue.

   function Get (Call_ID : in Call_ID_Type) return Call_Type;
   --  Returns the call with the specified Call_ID.

   function Dequeue (Call_ID : in Call_ID_Type) return Call_Type;
   --  Removes the call with the specified Call_ID and returns it.

   type Call_Process_Type is not null access procedure (Call : in Call_Type);

   package Call_List_Type is new
     Ada.Containers.Ordered_Maps (Key_Type   => Call_ID_Type,
                                  Element_Type => Call_Type);

   protected type Protected_Call_List_Type is
      function Contains (Call_ID : in Call_ID_Type) return Boolean;
      procedure Insert (Call : in Call_Type);
      procedure Remove (Call_ID : in Call_ID_Type);
      function Get (Call_ID : Call_ID_Type) return Call_Type;
      function Length return Long_Integer;
      function To_JSON return JSON_Value;
      function To_String return String;
      procedure For_Each (Process : in Call_Process_Type);
      procedure Update (Call : in Call_Type);
      --  Replaces the call at the ID location with the call supplied.
      --  Raises CALL_NOT_FOUND if there is no call to replace.

      function Next return Call_Type;
   private
      List : Call_List_Type.Map;
   end Protected_Call_List_Type;

   List : Protected_Call_List_Type;
   --  Package-visible singleton.
end Model.Calls;
