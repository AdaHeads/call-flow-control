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

with Ada.Exceptions, Ada.Text_IO; use Ada.Exceptions, Ada.Text_IO;

package body Receptions.Conditions.Clock is
   not overriding
   function Create (From, To : in String) return Instance is
      function Value (Elapsed_Time : in String)
        return Ada.Calendar.Day_Duration;

      function Value (Elapsed_Time : in String)
        return Ada.Calendar.Day_Duration is
      begin
         if Elapsed_Time'Length = 3 then
            return Ada.Calendar.Formatting.Value (Elapsed_Time & ":00:00");
         elsif Elapsed_Time'Length = 5 then
            return Ada.Calendar.Formatting.Value (Elapsed_Time & ":00");
         elsif Elapsed_Time'Length = 8 then
            return Ada.Calendar.Formatting.Value (Elapsed_Time);
         else
            raise Constraint_Error
              with """" & Elapsed_Time &
                   """ is not a properly formatted timestamp.";
         end if;
      end Value;
   begin
      return Result : Instance do
         Result := (From => Value (Elapsed_Time => From),
                    To   => Value (Elapsed_Time => To));

         if Result.From >= Result.To then
            raise Constraint_Error
              with From & " >= " & To & " (as timestamps).";
         end if;
      end return;
   exception
      when Constraint_Error =>
         raise Constraint_Error
           with "Failed to generate Ada object from <" & XML_Element_Name &
                " from=""" & From & """ to=""" & To & """>.";
      when E : others =>
         Put_Line (File => Standard_Error,
                   Item => "Receptions.Conditions.Clock.Create raised " &
                           Exception_Name (E) & " with " &
                           Exception_Message (E) & ".");
         raise;
   end Create;

   overriding
   function True (Item : in Instance;
                  Call : in PBX.Call.Identification) return Boolean is
      pragma Unreferenced (Call);
      use Ada.Calendar;
      Now : constant Day_Duration := Seconds (Ada.Calendar.Clock);
   begin
      return Item.From < Now and Now < Item.To;
   end True;

   overriding
   function Value (Item : in Instance) return String is
      use Ada.Calendar;
   begin
      return "Clock in " & Day_Duration'Image (Item.From) & " .. " &
                           Day_Duration'Image (Item.To);
   end Value;
end Receptions.Conditions.Clock;
