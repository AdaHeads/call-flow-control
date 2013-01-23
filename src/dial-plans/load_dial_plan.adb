with Ada.Command_Line;         use Ada.Command_Line;
with Ada.Text_IO.Text_Streams; use Ada.Text_IO, Ada.Text_IO.Text_Streams;

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

   Write (Stream       => Stream (Standard_Output),
          N            => Doc,
          Pretty_Print => True);

   Free (Reader);

   declare
      Dial_Plan         : Node := Get_Element (Doc);
      Seen_An_End_Point : Boolean := False;
   begin
   end;
end Load_Dial_Plan;
