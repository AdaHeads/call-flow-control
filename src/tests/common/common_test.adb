-------------------------------------------------------------------------------
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

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Expect;
with GNAT.OS_Lib;

with Common;

procedure Common_Test is
   use Ada.Command_Line;
   use Ada.Exceptions;
   use Ada.Strings.Unbounded;
   use Ada.Text_IO;
   use Common;
   use GNAT.Expect;
   use GNAT.OS_Lib;

   Exit_Code : Exit_Status := Success;

   Now_Ada         : constant Time := Current_Time;
   Now_String      : constant String := Unix_Timestamp (Now_Ada);
   Now_JSON_String : constant JSON_String := To_JSON_String (Now_String);
   Now_U_String    : constant Unbounded_String := U (Now_String);

   Cmd  : constant String := "date -u +%s";
   Args : constant Argument_List_Access := Argument_String_To_List (Cmd);
   Code : aliased Integer;
   Date : constant String := Get_Command_Output
     (Command    => Args (Args'First).all,
      Arguments  => Args (Args'First + 1 .. Args'Last),
      Input      => "",
      Status     => Code'Access);

   Long_Date    : constant Long_Integer := Long_Integer'Value (Date);
   Long_Now_Ada : constant Long_Integer := Long_Integer'Value (Now_String);
begin
   if To_String (Now_JSON_String) /= Now_String then
      Exit_Code := Failure;
      Put_Line ("Now_JSON_String /= Now_String");
   end if;

   if To_String (Null_JSON_String) /= "" then
      Exit_Code := Failure;
      Put_Line ("Null_JSON_String not empty");
   end if;

   if To_String (Now_U_String) /= Now_String then
      Exit_Code := Failure;
      Put_Line ("Now_U_String /= Now_String");
   end if;

   if Long_Date - Long_Now_Ada > 5 then
      Exit_Code := Failure;
      Put_Line (Date & " is more than 5 seconds off from " & Now_String);
   end if;

   Set_Exit_Status (Exit_Code);
exception
   when E : others =>
      Put_Line (Exception_Information (E));
      Set_Exit_Status (Failure);
end Common_Test;
