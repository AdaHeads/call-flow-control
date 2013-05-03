-------------------------------------------------------------------------------
--                                                                           --
--                      Copyright (C) 2012-, AdaHeads K/S                    --
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

with Ada.Command_Line,
     Ada.Exceptions,
     Ada.Text_IO;
with Receptions.No_PBX,
     Yolk.Command_Line;
with Model.Dial_Plans;

procedure Test_Dial_Plan is
   use Ada.Command_Line, Ada.Exceptions, Ada.Text_IO;
begin
   Set_Exit_Status (Success);

   Loading_Dial_Plans_From_Database :
   begin
      Model.Dial_Plans.Container.Reload_All;
   exception
      when E : others =>
         Set_Exit_Status (Failure);
         Put_Line
           (File => Standard_Error,
            Item => "Test_Dial_Plan terminated by an unexpected exception " &
                    "while loading dial-plans from the database: " &
                    Exception_Name (E) & " (" & Exception_Message (E) & ")");
         return;
   end Loading_Dial_Plans_From_Database;

   for Number of Yolk.Command_Line.Get ("--phone-number") loop
      begin
         Put_Line (Item => """" & Number & """ goes to the end-point """ &
                           Model.Dial_Plans.Container.End_Point
                             (Number => Number,
                              Call   => Receptions.No_PBX.Null_Call).Value);
      exception
         when E : others =>
            Set_Exit_Status (Failure);
            Put_Line
              (File => Standard_Error,
               Item => "Test_Dial_Plan terminated by an unexpected " &
                       "exception while testing the phone number """ & Number &
                       """: " & Exception_Name (E) & " (" &
                       Exception_Message (E) & ")");
      end;
   end loop;
exception
   when E : others =>
      Set_Exit_Status (Failure);
      Put_Line
        (File => Standard_Error,
         Item => "Test_Dial_Plan terminated by an unexpected exception: " &
                 Exception_Name (E) & " (" & Exception_Message (E) & ")");
end Test_Dial_Plan;
