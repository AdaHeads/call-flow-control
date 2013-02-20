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

with Ada.Calendar,
     Ada.Strings.Fixed,
     Ada.Strings.Unbounded;

package body Receptions.Conditions.DK.Week_Number is
   not overriding
   function Create (List : in String) return Instance is
      use Ada.Strings.Fixed,
          Calendars.DK;

      From  : Positive := List'First;
      Comma : Natural;
   begin
      return Result : Instance do
         loop
            Comma := Index (Source  => List (From .. List'Last),
                            Pattern => ",");

            exit when Comma = 0;

            Result.Weeks (Week_Numbers'Value (List (From .. Comma - 1))) :=
              True;

            From := Comma + 1;
         end loop;

         Result.Weeks (Week_Numbers'Value (List (From .. List'Last))) := True;
      end return;
   end Create;

   overriding
   function Evaluate (Item : in Instance;
                      Call : in Channel_ID) return Boolean is
      pragma Unreferenced (Call);
   begin
      return
        Item.Weeks (Calendars.DK.Week_Number (Ada.Calendar.Clock));
   end Evaluate;

   overriding
   function Value (Item : in Instance) return String is
      use Ada.Strings.Unbounded,
          Calendars.DK;
      Result : Unbounded_String;
      Prefix : Unbounded_String := To_Unbounded_String
                                     ("Danish week number in {");
   begin
      for Week in Item.Weeks'Range loop
         if Item.Weeks (Week) then
            Append (Result, Prefix);
            Append (Result, Week_Numbers'Image (Week));
            Prefix := To_Unbounded_String (",");
         end if;
      end loop;

      if Length (Result) = 0 then
         return "False";
      else
         return To_String (Result) & "}";
      end if;
   end Value;
end Receptions.Conditions.DK.Week_Number;
