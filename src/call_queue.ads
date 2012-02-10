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
with Ada.Strings.Unbounded;

package Call_Queue is

   type Call is limited private;
   type Priority_Level is (Low, Normal, High);
   subtype Call_Id is String (1 .. 10);

   procedure Add
     (Id         : in Call_Id;
      Callee     : in String;
      Caller     : in String;
      Priority   : in Priority_Level := Normal;
      Start      : in Ada.Calendar.Time);

   procedure Remove
     (A_Call : in Call);

   function Get
     return String;

private

   use Ada.Strings.Unbounded;

   type Call is
      record
         Id       : Call_Id;
         Callee   : Unbounded_String := Null_Unbounded_String;
         Caller   : Unbounded_String := Null_Unbounded_String;
         Priority : Priority_Level := Normal;
         Start    : Ada.Calendar.Time;
      end record;

end Call_Queue;
