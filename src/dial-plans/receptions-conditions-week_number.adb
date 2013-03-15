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

with Ada.Exceptions, Ada.Text_IO; use Ada.Exceptions, Ada.Text_IO;

package body Receptions.Conditions.Week_Number is
   function Week_Number (Date : in Ada.Calendar.Time) return Week_Numbers;

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

            Result.Weeks (Week_Numbers'Value (List (From .. Comma - 1)))
              := True;

            From := Comma + 1;
         end loop;

         Result.Weeks (Week_Numbers'Value (List (From .. List'Last))) := True;
      end return;
   exception
      when E : others =>
         Put_Line (File => Standard_Error,
                   Item => "Receptions.Conditions.Week_Number.Create raised " &
                           Exception_Name (E) & " with " &
                           Exception_Message (E) & ".");
         raise;
   end Create;

   overriding
   function True (Item : in Instance;
                  Call : in PBX.Call.Identification) return Boolean is
      pragma Unreferenced (Call);
   begin
      return
        Item.Weeks (Week_Number (Ada.Calendar.Clock));
   end True;

   overriding
   function Value (Item : in Instance) return String is
      use Ada.Calendar,
          Ada.Strings.Unbounded;
      Result : Unbounded_String;
      Prefix : Unbounded_String := To_Unbounded_String ("Week number in {");
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

   function Week_Number (Date : in Ada.Calendar.Time) return Week_Numbers is
      Year  : constant Ada.Calendar.Year_Number  := Ada.Calendar.Year  (Date);
      Month : constant Ada.Calendar.Month_Number := Ada.Calendar.Month (Date);
      Day   : constant Ada.Calendar.Day_Number   := Ada.Calendar.Day   (Date);

      --  See <http://www.tondering.dk/claus/cal/week.php#weekno>.
      a, b, c, s, e, f, g, d, n : Integer;
   begin
      case Month is
         when 1 .. 2 =>
            a := Year - 1;
            b :=       (a / 4) -       (a / 100) +       (a / 400);
            c := ((a - 1) / 4) - ((a - 1) / 100) + ((a - 1) / 400);
            s := b - c;
            e := 0;
            f := Day - 1 + 31 * (Month - 1);
         when 3 .. 12 =>
            a := Year;
            b :=       (a / 4) -       (a / 100) +       (a / 400);
            c := ((a - 1) / 4) - ((a - 1) / 100) + ((a - 1) / 400);
            s := b - c;
            e := s + 1;
            f := Day + ((153 * (Month - 3) + 2) / 5) + 58 + s;
      end case;

      g := (a + b) mod 7;
      d := (f + g - e) mod 7;
      n := f + 3 - d;

      if n < 0 then
         return 53 - ((g - s) / 5);
      elsif n > 364 + s then
         return 1;
      else
         return (n / 7) + 1;
      end if;
   end Week_Number;
end Receptions.Conditions.Week_Number;
