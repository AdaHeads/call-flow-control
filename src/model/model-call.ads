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

with Common;

package Model.Call is
   use Ada.Strings.Unbounded;
   use Common;

   type Call_State is
     (Unknown, Queued, Speaking, Ringing, OnHold, Delegated, Hung);
   type Priority_Level is (Low, Normal, High);

   type Call_ID_Type is record
      Timestamp : Integer;
      Sequence  : Integer;
   end record;
--      new Long_Integer range -1 .. (2**63)-1;
   Null_Call_ID : constant Call_ID_Type := (-1,-1);
   
   type Call_Type is
      record
         ID             : Call_ID_Type;
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
      end record;

   function Image return String;
   --  Returns a debug friendly String representation of the call queue.
   
   function "=" (Left  : in Call_Type;
                 Right : in Call_Type) return Boolean;
   function "<" (Left  : in Call_ID_Type;
                 Right : in Call_ID_Type) return Boolean;
      
   
   package Call_List_Type is new
     Ada.Containers.Ordered_Maps (Key_Type   => Call_ID_Type,
                                  Element_Type => Call_Type);
   --  ???? Naming is a bit "odd". This is not really a type as such, but a
   --  package containing the Vector type. Perhaps a better solution would be
   --  to rename the parent package from Call_List to Calls and this package
   --  to Queue, then you'd have a naming scheme like this:
   --      Calls.Queue.Vector
   --  instead of
   --      Call_List.Call_List_Type.Vector

   Null_Call : constant Call_Type :=
     (ID             => Null_Call_ID,
      State          => Hung,
      Queue_Priority => Low,
      Inbound        => False,
      Extension      => Null_Unbounded_String,
      Channel        => Null_Unbounded_String,
      CallerIDNum    => Null_Unbounded_String,
      CallerIDName   => Null_Unbounded_String,
      Queue          => Null_Unbounded_String,
      Position       => 0,
      Count          => 0,
      Arrived        => Current_Time);

   procedure Insert (Call : in Call_Type);
   --  Places a call in the call_List.

   procedure Remove (Call_ID : in Call_ID_Type);
   --  Removes a specific call from the call queue.

   function Get (Call_ID : in Call_ID_Type) return Call_Type;
   --  Returns the call with the specified Call_ID.
   
   function Get_List return Call_List_Type.Map;
   --  Returns the entire list;
   
   function Dequeue (Call_ID : in Call_ID_Type) return Call_Type;
   --  Removes the call with the specified Call_ID and returns it.

   procedure Update (Call : in Call_Type);
   --  Updates a Call in the list, with the information from the Call Argument

   function Length return Long_Integer;
   --  Gives the length of the call queue.

   function To_Call_ID (Item : String) return Call_ID_Type;
   --  Convenience method.

   function Next return Call_Type;

   function To_String (Call : in Call_Type) return String;
   function To_String (Call_ID : in Call_ID_Type) return String;
private

   protected Protected_Call_List is
      procedure Insert (Call : in Call_Type);
      procedure Remove (Call_ID : in Call_ID_Type);
      function Get_List return Call_List_Type.Map;
      function Get (Call_ID : Call_ID_Type) return Call_Type;
      function Length return Long_Integer;
--      function For_Each return String;
      procedure Update (Call : in Call_Type);
      function Next return Call_Type;
   private
      List : Call_List_Type.Map;
   end Protected_Call_List;
end Model.Call;
