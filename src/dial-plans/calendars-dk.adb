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

with Ada.Calendar.Formatting;

with System_Message.Critical;

package body Calendars.DK is
   function Official_Holiday (Date : in Ada.Calendar.Time) return Boolean is
      Year  : constant Ada.Calendar.Year_Number  := Ada.Calendar.Year  (Date);
      Month : constant Ada.Calendar.Month_Number := Ada.Calendar.Month (Date);
      Day   : constant Ada.Calendar.Day_Number   := Ada.Calendar.Day   (Date);
      use Ada.Calendar.Formatting;
   begin
      System_Message.Critical.Partial_Calendar_Data;

      return
        (Day_Of_Week (Date) = Sunday) or else -- Sunday
        (Month = 1  and then Day = 1) or else -- New Year
        (Month = 12 and then Day in 25 .. 26); --  Christmas
   end Official_Holiday;

   function Banking_Day (Date : in Ada.Calendar.Time) return Boolean is
      Year  : constant Ada.Calendar.Year_Number  := Ada.Calendar.Year  (Date);
      Month : constant Ada.Calendar.Month_Number := Ada.Calendar.Month (Date);
      Day   : constant Ada.Calendar.Day_Number   := Ada.Calendar.Day   (Date);
   begin
      System_Message.Critical.Partial_Calendar_Data;

      if Official_Holiday (Date) then
         return False;
      else
         return (Month = 5 and then Day = 1); -- 1st of May
      end if;
   end Banking_Day;

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
end Calendars.DK;
