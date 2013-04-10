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

with Ada.Strings.Fixed,
     Ada.Strings.Unbounded;

package body Receptions.Conditions.Month is
   not overriding
   function Create (List : in String) return Instance is
      use Ada.Calendar,
          Ada.Strings.Fixed;

      From  : Positive := List'First;
      Comma : Natural;
   begin
      return Result : Instance do
         loop
            Comma := Index (Source  => List (From .. List'Last),
                            Pattern => ",");

            exit when Comma = 0;

            Result.Months (Month_Number'Value (List (From .. Comma - 1))) :=
                                                                          True;

            From := Comma + 1;
         end loop;

         Result.Months (Month_Number'Value (List (From .. List'Last))) := True;
      end return;
   exception
      when Constraint_Error =>
         raise Constraint_Error
           with "Receptions.Conditions.Month: " &
                "Failed to parse list of month numbers: """ & List & """.";
   end Create;

   overriding
   function True (Item : in Instance;
                  Call : in PBX.Call.Identification) return Boolean is
      pragma Unreferenced (Call);
   begin
      return
        Item.Months (Ada.Calendar.Month (Ada.Calendar.Clock));
   end True;

   overriding
   function Value (Item : in Instance) return String is
      use Ada.Calendar,
          Ada.Strings.Unbounded;
      Result : Unbounded_String;
      Prefix : Unbounded_String := To_Unbounded_String ("Month number in {");
   begin
      for Month in Item.Months'Range loop
         if Item.Months (Month) then
            Append (Result, Prefix);
            Append (Result, Month_Number'Image (Month));
            Prefix := To_Unbounded_String (",");
         end if;
      end loop;

      if Length (Result) = 0 then
         return "False";
      else
         return To_String (Result) & "}";
      end if;
   end Value;
end Receptions.Conditions.Month;
