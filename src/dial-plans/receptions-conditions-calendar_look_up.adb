-------------------------------------------------------------------------------
--                                                                           --
--                      Copyright (C) 2013-, AdaHeads K/S                    --
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

with System_Message.Critical;

package body Receptions.Conditions.Calendar_Look_Up is
   not overriding
   function Create (Kind : in String) return Instance is
   begin
      --  Would like to check if Kind exists in the calendar database.
      System_Message.Critical.No_Calendar_Database;
      return (Kind => Ada.Strings.Unbounded.To_Unbounded_String (Kind));
   end Create;

   overriding
   function Evaluate (Item : in Instance;
                      Call : in PBX.Call.Identification) return Boolean is
      pragma Unreferenced (Call);
   begin
      --  Would like to look up today date in the calendar database.
      System_Message.Critical.No_Calendar_Database;
      return False;
   end Evaluate;

   overriding
   function Value (Item : in Instance) return String is
   begin
      return "Looking up """ & Ada.Strings.Unbounded.To_String (Item.Kind) &
             """ in calendar.";
   end Value;
end Receptions.Conditions.Calendar_Look_Up;