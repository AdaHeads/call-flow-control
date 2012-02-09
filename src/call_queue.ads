-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                               Call_Queue                                  --
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
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;

package Call_Queue is
private

   use Ada.Containers;
   use Ada.Strings.Unbounded;

   type Id_Type is mod 2 ** 32;
   --  This should be plenty big to avoid Id collisions in the queue.

   type Importance_Level is (Low, Normal, High);

   type Call is
      record
         Id         : Id_Type;
         Caller     : Unbounded_String := Null_Unbounded_String;
         Company    : Unbounded_String := Null_Unbounded_String;
         Importance : Importance_Level := Normal;
         Start      : Ada.Calendar.Time;
      end record;

   package Call_Queue_List is new Doubly_Linked_Lists (Call);
   use Call_Queue_List;

   protected Waiting_Calls is
      function Get return List;
      procedure Set
        (A_Call : in Call);
   private
      L : List;
   end Waiting_Calls;

end Call_Queue;
