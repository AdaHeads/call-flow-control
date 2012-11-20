-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                Call_List                                  --
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
with Ada.Strings.Unbounded;
with Model.Agent;
with Common;
with Model.Call_ID;
with GNATCOLL.JSON;

package Model.Call is
   use Ada.Strings.Unbounded;
   use GNATCOLL.JSON;
   use Common;
   use Model.Agent;
   use Model.Call_ID;

   CALL_NOT_FOUND  : exception;
   DUPLICATE_ID    : exception;
   BAD_EXTENSION   : exception;
   EMPTY_EXTENSION : exception;
     
   type Call_State is
     (Unknown, Newly_Arrived, Speaking, Ringing, OnHold, Delegated, Hung_Up);
   type Priority_Level is (Invalid, Low, Normal, High);

   type Call_Type is tagged
      record
         ID             : Call_ID.Call_ID_Type;
         State          : Call_State;
         Inbound        : Boolean;
         Extension      : Unbounded_String;
         Queue_Priority : Priority_Level;
         Channel        : Unbounded_String;
         CallerIDNum    : Unbounded_String;
         CallerIDName   : Unbounded_String;
         Queue          : Unbounded_String;
         Position       : Natural;
         Count          : Natural;
         Arrived        : Time := Current_Time;
         Assigned_To    : Agent_ID_Type := 0;
      end record;
   
   procedure Insert (Call : in Call_Type);
   --  Places a call in the call_List.

   procedure Remove (Call_ID : in Call_ID_Type);
   --  Removes a specific call from the call queue.

   procedure Update (Call : in Call_Type);
   --  Updates a Call in the list, with the information from the Call Argument

   function Length return Long_Integer;
   --  Gives the length of the call queue.

   function To_String (Call : in Call_Type) return String;
   function To_JSON (Call : in Call_Type) return JSON_Value;

   
   function "=" (Left  : in Call_Type;
                 Right : in Call_Type) return Boolean;

   function Get (Call_ID : in Call_ID_Type) return Call_Type;
   --  Returns the call with the specified Call_ID.
      
   function Dequeue (Call_ID : in Call_ID_Type) return Call_Type;
   --  Removes the call with the specified Call_ID and returns it.


   type Call_Process_Type is not null access procedure (Call : in Call_Type);

   Null_Call : constant Call_Type :=
     (ID             => Call_ID.Null_Call_ID,
      State          => Unknown,
      Queue_Priority => Invalid,
      Inbound        => False,
      Extension      => Null_Unbounded_String,
      Channel        => Null_Unbounded_String,
      CallerIDNum    => Null_Unbounded_String,
      CallerIDName   => Null_Unbounded_String,
      Queue          => Null_Unbounded_String,
      Position       => 0,
      Count          => 0,
      Arrived        => Current_Time,
      Assigned_To    => 0);

   package Call_List_Type is new
     Ada.Containers.Ordered_Maps (Key_Type   => Call_ID_Type,
                                  Element_Type => Call_Type);

   protected type Protected_Call_List_Type is 
      procedure Insert (Call : in Call_Type);
      procedure Remove (Call_ID : in Call_ID_Type);
      function Get (Call_ID : Call_ID_Type) return Call_Type;
      function Length return Long_Integer;
      function To_JSON return JSON_Value;
      function To_String return String;
      procedure For_Each (Process : in Call_Process_Type);
      procedure Update (Call : in Call_Type);
      function Next return Call_Type;
   private
      List : Call_List_Type.Map;
   end Protected_Call_List_Type;

   Call_List : Protected_Call_List_Type;
   -- Package-visible singleton.
end Model.Call;
