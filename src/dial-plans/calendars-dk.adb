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

with Ada.Calendar.Arithmetic,
     Ada.Calendar.Formatting;

with System_Message.Critical;

package body Calendars.DK is
   procedure Easter (Year  : in     Ada.Calendar.Year_Number;
                     Month :    out Ada.Calendar.Month_Number;
                     Day   :    out Ada.Calendar.Day_Number)
     with Precondition => (Year in 1900 .. 2099);

   procedure Easter (Year  : in     Ada.Calendar.Year_Number;
                     Month :    out Ada.Calendar.Month_Number;
                     Day   :    out Ada.Calendar.Day_Number) is
      H, I, J, L : Integer;
   begin
      H := (24 + 19 * (Year mod 19)) mod 30;
      I := H - (H / 28);
      J := (Year + (Year / 4) + I - 13) mod 7;
      L := I - J;
      Month := 3 + ((L + 40) / 44);
      Day   := L + 28 - 31 * (Month / 4);
   end Easter;

   function Easter
     (Date   : in Ada.Calendar.Time;
      Offset : in Ada.Calendar.Arithmetic.Day_Count) return Boolean;

   function Easter
     (Date   : in Ada.Calendar.Time;
      Offset : in Ada.Calendar.Arithmetic.Day_Count) return Boolean is
      use Ada.Calendar.Arithmetic;
      Year  : constant Ada.Calendar.Year_Number  := Ada.Calendar.Year
                                                      (Date - Offset);
      Month : constant Ada.Calendar.Month_Number := Ada.Calendar.Month
                                                      (Date - Offset);
      Day   : constant Ada.Calendar.Day_Number   := Ada.Calendar.Day
                                                      (Date - Offset);
      Easter_Month : Ada.Calendar.Month_Number;
      Easter_Day   : Ada.Calendar.Day_Number;
   begin
      Easter (Year  => Year,
              Month => Easter_Month,
              Day   => Easter_Day);
      return Month = Easter_Month and then Day = Easter_Day;
   end Easter;

   function Official_Holiday (Date : in Ada.Calendar.Time) return Boolean is
      Year  : constant Ada.Calendar.Year_Number  := Ada.Calendar.Year  (Date);
      Month : constant Ada.Calendar.Month_Number := Ada.Calendar.Month (Date);
      Day   : constant Ada.Calendar.Day_Number   := Ada.Calendar.Day   (Date);
      use Ada.Calendar.Formatting;
      use type Ada.Calendar.Arithmetic.Day_Count;
   begin
      return
        (Day_Of_Week (Date) = Sunday) or else -- Sunday
        (Month = 1  and then Day = 1) or else -- New Year
        (Easter (Date,  -3)) or else --  Skærtorsdag
        (Easter (Date,  -2)) or else --  Langfredag
        (Easter (Date,  +1)) or else --  Anden påskedag
        (Easter (Date, +26)) or else --  Store bededag
        (Easter (Date, +39)) or else --  Kristi himmelfartsdag
        (Easter (Date, +50)) or else --  Anden pinsedag
        (Month = 12 and then Day in 25 .. 26); --  Christmas
   end Official_Holiday;

   function Banking_Day (Date : in Ada.Calendar.Time) return Boolean is
      Year  : constant Ada.Calendar.Year_Number  := Ada.Calendar.Year  (Date);
      Month : constant Ada.Calendar.Month_Number := Ada.Calendar.Month (Date);
      Day   : constant Ada.Calendar.Day_Number   := Ada.Calendar.Day   (Date);
      use Ada.Calendar.Formatting;
   begin
      System_Message.Critical.Partial_Calendar_Data;

      if Official_Holiday (Date) then
         return False;
      elsif Day_Of_Week (Date) in Saturday .. Sunday then
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
