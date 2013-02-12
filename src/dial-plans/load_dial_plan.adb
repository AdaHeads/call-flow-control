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
     Ada.Strings.Unbounded,
     Ada.Text_IO;

with Input_Sources.File;       use Input_Sources.File;
with Sax.Readers;              use Sax.Readers;
with DOM.Readers;              use DOM.Readers;
with DOM.Core;                 use DOM.Core;
with DOM.Core.Documents;       use DOM.Core.Documents;
with DOM.Core.Nodes;           use DOM.Core.Nodes;

procedure Load_Dial_Plan is
   procedure Check (Element : in     Node;
                    Name    : in     String) is
   begin
      if Element = null then
         raise Constraint_Error with "No element.";
      elsif Node_Type (Element) /= Element_Node then
         raise Constraint_Error with "Not an element.";
      elsif Node_Name (Element) /= Name then
         raise Constraint_Error with "Not an <" & Name & "> element.";
      end if;
   end Check;

   Input  : File_Input;
   Reader : Tree_Reader;
   Doc    : Document;

   Dial_Plan_Title    : Ada.Strings.Unbounded.Unbounded_String;
   Start_Action_Title : Ada.Strings.Unbounded.Unbounded_String;
begin
   Set_Public_ID (Input, "dial-plan");

   Open_Source_File:
   declare
      use Ada.Command_Line, Ada.Text_IO;
   begin
      if Argument_Count = 1 then
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

   declare
      Dial_Plan : Node := Get_Element (Doc);
   begin
      Check (Element => Dial_Plan, Name => "dial-plan");

      declare
         use Ada.Strings.Unbounded;
         Title_Attribute : not null Node := Get_Named_Item (Nodes.Attributes (Dial_Plan), "title");
      begin
         Dial_Plan_Title := To_Unbounded_String (Node_Value (Title_Attribute));
      end;

      declare
         Child : Node := First_Child (Dial_Plan);
      begin
         loop
            exit when Child = null;
            exit when Node_Type (Child) = Element_Node;
            Child := Next_Sibling (Child);
         end loop;

         Check (Element => Child, Name => "start");

         declare
            use Ada.Strings.Unbounded;
            Do_Attribute : not null Node := Get_Named_Item (Nodes.Attributes (Child), "do");
         begin
            Start_Action_Title := To_Unbounded_String (Node_Value (Do_Attribute));
         end;
      end;
   end;

   declare
      use Ada.Strings.Unbounded, Ada.Text_IO;
   begin
      Put_Line ("Dial-plan title:       """ & To_String (Dial_Plan_Title) & """");
      Put_Line ("Title of first action: """ & To_String (Start_Action_Title) & """");
   end;
end Load_Dial_Plan;
