with Ada.Command_Line;         use Ada.Command_Line;
with Ada.Text_IO.Text_Streams; use Ada.Text_IO, Ada.Text_IO.Text_Streams;

with Input_Sources.File;       use Input_Sources.File;
with Sax.Readers;              use Sax.Readers;
with DOM.Readers;              use DOM.Readers;
with DOM.Core;                 use DOM.Core;
with DOM.Core.Documents;       use DOM.Core.Documents;
with DOM.Core.Nodes;           use DOM.Core.Nodes;

procedure Normalise_Dial_Plan is
   Input  : File_Input;
   Reader : Tree_Reader;
   Doc    : Document;
begin
   if Argument_Count = 1 then
      Set_Public_Id (Input, "Dial-plan");

      begin
         Open (Argument (1), Input);
      exception
         when others =>
            Put_Line (Standard_Error, Command_Name & " failed to open '" & Argument (1) & "'.");
            Set_Exit_Status (Failure);
            return;
      end;

      Set_Feature (Reader, Validation_Feature, True);
      Set_Feature (Reader, Namespace_Feature, False);

      Parse (Reader, Input);
      Close (Input);

      Doc := Get_Tree (Reader); 

      Write (Stream       => Stream (Standard_Output),
             N            => Doc,
             Pretty_Print => True);
   else
      Put_Line (Standard_Error, "Usage:");
      Put_Line (Standard_Error, "   " & Command_Name & " <dial-plan_file>");
      Set_Exit_Status (Failure);
   end if;
end Normalise_Dial_Plan;
