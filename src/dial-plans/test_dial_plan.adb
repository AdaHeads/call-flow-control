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

with Input_Sources.File;       use Input_Sources.File;
with Sax.Readers;              use Sax.Readers;
with DOM.Readers;              use DOM.Readers;
with DOM.Core;                 use DOM.Core;
with DOM.Core.Documents;       use DOM.Core.Documents;

with AMI.Response,
     PBX,
     PBX.Call,
     Receptions.Dial_Plan,
     Receptions.Dial_Plan.IO;

procedure Test_Dial_Plan is
   Input     : File_Input;
   Reader    : Tree_Reader;
   Doc       : Document;
   Reception : Receptions.Dial_Plan.Instance;
begin
   PBX.Start;

   Set_Public_Id (Input, Receptions.Dial_Plan.XML_Element_Name);
   --  ID should be all upper-case, but AdaCore doesn't seem to get that.

   Open_Source_File :
   declare
      use Ada.Command_Line, Ada.Text_IO;
   begin
      if Argument_Count >= 1 then
         Open (Argument (1), Input);
      else
         Put_Line (Standard_Error, "Usage:");
         Put_Line (Standard_Error, "   " & Command_Name & " <dial-plan file>");
         Set_Exit_Status (Failure);
         return;
      end if;
   exception
      when others =>
         Set_Exit_Status (Failure);
         return;
   end Open_Source_File;

   Set_Feature (Reader, Validation_Feature, True);
   Set_Feature (Reader, Namespace_Feature, False);

   Parse (Reader, Input);
   Close (Input);

   Doc := Get_Tree (Reader);

   Reception := Receptions.Dial_Plan.IO.Load (From => Get_Element (Doc));

   Ada.Text_IO.Put_Line
     (Item => "Dial-plan title: " & Reception.Title);
   Ada.Text_IO.Put_Line
     (Item => "Current action:  " &
              Reception.Application
                (Call => PBX.Call.Null_Identification).Value);

   PBX.Stop;
exception
   when AMI.Response.Timeout =>
      null;
   when E : others =>
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      Ada.Text_IO.Put_Line
        (File => Ada.Text_IO.Standard_Error,
         Item => "Test_Dial_Plan terminated by an unexpected exception: " &
                 Ada.Exceptions.Exception_Name (E) & " (" &
                 Ada.Exceptions.Exception_Message (E) & ")");
      PBX.Stop;
end Test_Dial_Plan;
