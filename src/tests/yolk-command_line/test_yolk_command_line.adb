-------------------------------------------------------------------------------
--                                                                           --
--                     Copyright (C) 2013-, AdaHeads K/S                     --
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
     Ada.Containers,
     Ada.Exceptions,
     Ada.Text_IO;

with Yolk.Command_Line;

procedure Test_Yolk_Command_Line is
   use Ada.Command_Line,
       Ada.Text_IO,
       Yolk.Command_Line;
   use type Ada.Containers.Count_Type;

   procedure Failed (Message : in     String);
   procedure Failed (Message : in     String) is
   begin
      if Get ("--set-exit-status", "no") = "yes" then
         Set_Exit_Status (Failure);
      end if;

      Put_Line (Standard_Error, Message);
   end Failed;
begin

   Set_Exit_Status :
   declare
      Choice : String_Vectors.Vector;
   begin
      Choice := Get ("--set-exit-status");

      if Choice.Length /= 1 then
         Failed ("Expected 1 parameter after '--set-exit-status'.");
      end if;
   end Set_Exit_Status;

   Three_Files :
   declare
      File_Names : String_Vectors.Vector;
   begin
      File_Names := Get ("--three-files");

      if File_Names.Length /= 3 then
         Failed ("Expected 3 file names after '--three-files'.");
      end if;
   end Three_Files;

exception
   when E : others =>
      Failed (Message => "An exception was raised: " &
                         Ada.Exceptions.Exception_Information (E));
end Test_Yolk_Command_Line;
