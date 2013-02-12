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

with Ada.Command_Line;         use Ada.Command_Line;
with Ada.Text_IO;              use Ada.Text_IO;

with Input_Sources.File;       use Input_Sources.File;
with Sax.Readers;              use Sax.Readers;
with DOM.Readers;              use DOM.Readers;
with DOM.Core;                 use DOM.Core;
with DOM.Core.Documents;       use DOM.Core.Documents;
with DOM.Core.Nodes;           use DOM.Core.Nodes;

procedure Load_Dial_Plan is
   Input  : File_Input;
   Reader : Tree_Reader;
   Doc    : Document;
begin
   Set_Public_Id (Input, "Dial-plan");
   Open (Argument (1), Input);

   Set_Feature (Reader, Validation_Feature, True);
   Set_Feature (Reader, Namespace_Feature, False);

   Parse (Reader, Input);
   Close (Input);

   Doc := Get_Tree (Reader); 

   declare
      Dial_Plan         : Node := Get_Element (Doc);
      Attributes        : Named_Node_Map := Nodes.Attributes (Dial_Plan);
      Title             : Node := Get_Named_Item (Attributes, "title");
      Seen_An_End_Point : Boolean := False;
   begin
      Put_Line ("Dial-plan title: """ & Node_Value (Title) & """");
      Put_Line ("Needs <start> element too.");
   end;
end Load_Dial_Plan;
