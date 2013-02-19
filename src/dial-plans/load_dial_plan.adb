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

with Receptions.Decision_Tree_Collection,
     Receptions.Dial_Plan,
     Receptions.End_Point_Collection,
     Receptions.End_Points.Hang_Up,
     Receptions.End_Points.Queue;

procedure Load_Dial_Plan is
   procedure Check (Element : in     Node;
                    Name    : in     String);
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

   Reception : Receptions.Dial_Plan.Instance;

   Dial_Plan_Title    : Ada.Strings.Unbounded.Unbounded_String;
   Start_Action_Title : Ada.Strings.Unbounded.Unbounded_String;
   End_Points         : Receptions.End_Point_Collection.Map;
   Decision_Trees     : Receptions.Decision_Tree_Collection.Map;
begin
   Set_Public_ID (Input, "dial-plan");

   Open_Source_File :
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
      Dial_Plan : constant Node := Get_Element (Doc);
   begin
      Check (Element => Dial_Plan, Name => "dial-plan");

      declare
         use Ada.Strings.Unbounded;
         Title_Attribute : constant not null Node :=
                             Get_Named_Item (Nodes.Attributes (Dial_Plan),
                                             "title");
      begin
         Dial_Plan_Title := To_Unbounded_String (Node_Value (Title_Attribute));
      end;

      declare
         Start : Node := First_Child (Dial_Plan);
      begin
         loop
            exit when Start = null;
            exit when Node_Type (Start) = Element_Node;
            Start := Next_Sibling (Start);
         end loop;

         Check (Element => Start, Name => "start");

         declare
            use Ada.Strings.Unbounded;
            Do_Attribute : constant not null Node :=
                             Get_Named_Item (Nodes.Attributes (Start), "do");
         begin
            Start_Action_Title :=
              To_Unbounded_String (Node_Value (Do_Attribute));
         end;
      end;

      declare
         End_Point : Node := First_Child (Dial_Plan);
      begin
         Find_End_Points :
         loop
            Next_End_Point :
            loop
               exit Find_End_Points when End_Point = null;
               exit Next_End_Point when Node_Type (End_Point) = Element_Node and then
                                        Node_Name (End_Point) = "end-point";
               End_Point := Next_Sibling (End_Point);
            end loop Next_End_Point;

            Check (Element => End_Point, Name => "end-point");

            declare
               use Ada.Strings.Unbounded, Ada.Text_IO;
               Title_Attribute : constant not null Node :=
                                   Get_Named_Item (Nodes.Attributes (End_Point), "title");
               Title           : constant String := Node_Value (Title_Attribute);
            begin
               declare
                  End_Point_Action : not null Node := First_Child (End_Point);
               begin
                  loop
                     exit when End_Point_Action = null;
                     exit when Node_Type (End_Point_Action) = Element_Node;
                     End_Point_Action := Next_Sibling (End_Point_Action);
                  end loop;

                  if Node_Name (End_Point_Action) = "hang-up" then
                     Put_Line ("End-point type:        hang-up");

                     declare
                        package Hang_Up renames Receptions.End_Points.Hang_Up;
                     begin
                        End_Points.Insert (Key      => Title,
                                           New_Item => Hang_Up.Create (Title => Title));
                     end;
                  elsif Node_Name (End_Point_Action) = "queue" then
                     Put_Line ("End-point type:        queue");

                     declare
                        use Receptions.End_Points.Queue;

                        ID_Attribute : constant not null Node :=
                                         Get_Named_Item (Nodes.Attributes (End_Point_Action),
                                                                        "id");
                        Queue : Receptions.End_Points.Queue.Instance;
                     begin
                        Queue := Create (Title => Title,
                                         ID    => Node_Value (ID_Attribute));
                        End_Points.Insert (Key      => Title,
                                           New_Item => Queue);
                     end;
                  elsif Node_Name (End_Point_Action) = "redirect" then
                     Put_Line ("End-point type:        redirect");
                  elsif Node_Name (End_Point_Action) = "interactive-voice-response" then
                     Put_Line ("End-point type:        interactive-voice-response");
                  elsif Node_Name (End_Point_Action) = "voice-mail" then
                     Put_Line ("End-point type:        voice-mail");
                  elsif Node_Name (End_Point_Action) = "busy-signal" then
                     Put_Line ("End-point type:        busy-signal");
                  else
                     raise Constraint_Error with "<end-point> element contains illegal element <" &
                                                 Node_Name (End_Point_Action) & ">.";
                  end if;
               end;

               Put_Line ("End-point title:       """ & Node_Value (Title_Attribute) & """");
            end;

            End_Point := Next_Sibling (End_Point);
         end loop Find_End_Points;
      end;
   end;

   declare
      use Ada.Strings.Unbounded;
   begin
      Reception := Receptions.Dial_Plan.Create
                     (Title          => To_String (Dial_Plan_Title),
                      Start_At       => To_String (Start_Action_Title),
                      End_Points     => End_Points,
                      Decision_Trees => Decision_Trees);
   end;

   declare
      use Ada.Strings.Unbounded, Ada.Text_IO;
   begin
      Put_Line ("Dial-plan title:       """ & Reception.Title & """");
      Put_Line ("Title of first action: """ & To_String (Start_Action_Title) & """");
   end;
end Load_Dial_Plan;
