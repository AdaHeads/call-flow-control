with Ada.Containers;
with Ada.Calendar;
with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
--  with Ada.Containers.Doubly_Linked_Lists;
package Call_Queue is
   use Ada.Strings.Unbounded;
   --     Type Call_Queue_Type is private;

   type Call_Type is
      record
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

   procedure printCall (Call : in Call_Type);

   package Queue_Type is new
     Ada.Containers.Vectors (Index_Type => Positive,
                             Element_Type => Call_Type);

   type Priority_Level is (Low, Normal, High);

   type Call_Queue_Type is array (Priority_Level) of Queue_Type.Vector;

   null_Call : constant Call_Type := (Channel => Null_Unbounded_String,
                                      CallerIDNum => Null_Unbounded_String,
                                      CallerIDName => Null_Unbounded_String,
                                      Queue => Null_Unbounded_String,
                                      Position => 0,
                                      Count => 0,
                                      Uniqueid => Null_Unbounded_String,
                                      Arrived => Ada.Calendar.Clock,
                                      Picked_Up => Ada.Calendar.Clock,
                                      Is_Picked_Up => False,
                                      Ended => Ada.Calendar.Clock,
                                      Is_Ended => False);

   procedure Enqueue (Call : in Call_Type);
   --  Places a call on the call queue.

   procedure Dequeue (Call : out Call_Type);
   --  Takes the first call with the highest priority

   procedure Dequeue (Uniqueid : in     Unbounded_String;
                      Call     :    out Call_Type);
   --  Takes a specific call out from the call queue.

   procedure Remove (Uniqueid : in Unbounded_String);
   --  Removes a specific call from the call queue.

   function Get_Queue return Call_Queue_Type;
   --  Returns the entire call queue.

   function Queue_Length return Ada.Containers.Count_Type;
   --  Gives the length of the call queue.

   function Queue_ToString return Unbounded_String;
   --  Returns a debug friendly String representation of the call queue.

private

   function Get_Company_Priority (CompanyName : in Unbounded_String)
                                  return Priority_Level;
end Call_Queue;
