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

with Ada.Calendar;
with Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

package Call_List is
   use Ada.Strings.Unbounded;

   type Call_State is
     (Unknown, Queued, Speaking, Ringing, OnHold, Delegated, Hung);
   type Priority_Level is (Low, Normal, High);

   type Call_Type is
      record
         Agent_ID       : Unbounded_String;
         State          : Call_State;
         Inbound        : Boolean;
         Extension      : Unbounded_String;
         Queue_Priority : Priority_Level;
         Channel        : Unbounded_String;
         CallerIDNum    : Unbounded_String;
         CallerIDName   : Unbounded_String;
         Queue          : Unbounded_String;
         Position       : Integer;
         Count          : Integer;
         Uniqueid       : Unbounded_String;
         Arrived        : Ada.Calendar.Time := Ada.Calendar.Clock;
         Is_Picked_Up   : Boolean := False;
         Picked_Up      : Ada.Calendar.Time;
         Is_Ended       : Boolean := False;
         Ended          : Ada.Calendar.Time;
      end record;

   function Image (Call : in Call_Type) return String;
   function Image return String;
   --  Returns a debug friendly String representation of the call queue.

   package Call_List_Type is new
     Ada.Containers.Vectors (Index_Type   => Positive,
                             Element_Type => Call_Type);
   --  ???? Naming is a bit "odd". This is not really a type as such, but a
   --  package containing the Vector type. Perhaps a better solution would be
   --  to rename the parent package from Call_List to Calls and this package
   --  to Queue, then you'd have a naming scheme like this:
   --      Calls.Queue.Vector
   --  instead of
   --      Call_List.Call_List_Type.Vector

   Null_Call : constant Call_Type :=
     (Agent_ID       => Null_Unbounded_String,
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
      Uniqueid       => Null_Unbounded_String,
      Arrived        => Ada.Calendar.Clock,
      Picked_Up      => Ada.Calendar.Clock,
      Is_Picked_Up   => False,
      Ended          => Ada.Calendar.Clock,
      Is_Ended       => False);

   procedure Add (Call : in Call_Type);
   --  Places a call in the call_List.

--     procedure Dequeue (Call : out Call_Type);
--     --  Takes the first call with the highest priority
--
--     procedure Dequeue (Uniqueid : in     Unbounded_String;
--                        Call     :    out Call_Type);
--     --  Takes a specific call out from the call queue.

   function Remove (Uniqueid : in Unbounded_String) return Call_Type;
   --  Removes a specific call from the call queue.

   function Get return Call_List_Type.Vector;
   --  Returns the entire call queue.

   function Get_Call (UniqueID : in Unbounded_String) return Call_Type;
   --  Returns the call with the specified UniqueID.

   procedure Update (Call : in Call_Type);
   --  Updates a Call in the list, with the information from the Call Argument

   function Length return Ada.Containers.Count_Type;
   --  Gives the length of the call queue.
   
--     function PickupCall
--       (Agent_ID : in Unbounded_String;
--        Uniqueid : in Unbounded_String := Null_Unbounded_String)
--           return Call_Type;
--     --  Sets the queued call in speaking state.
--
--     function Hangup
--       (CallID : in Unbounded_String) return Call_Type;
   --
private

--     function Get_Company_Priority (CompanyName : in Unbounded_String)
--                                    return Priority_Level;
end Call_List;
