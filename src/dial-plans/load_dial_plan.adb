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

with AMI.Response,
     PBX,
     Receptions.Decision_Tree,
     Receptions.Decision_Tree_Collection,
     Receptions.Dial_Plan,
     Receptions.End_Point,
     Receptions.End_Point_Collection,
     Receptions.End_Points.Busy_Signal,
     Receptions.End_Points.Hang_Up,
     Receptions.End_Points.Interactive_Voice_Response,
     Receptions.End_Points.Queue,
     Receptions.End_Points.Redirect,
     Receptions.End_Points.Voice_Mail;

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

   function Attribute (Element : in     Node;
                       Name    : in     String) return String;
   function Attribute (Element : in     Node;
                       Name    : in     String) return String is
   begin
      if Element = null then
         raise Constraint_Error with "No element.";
      elsif Node_Type (Element) /= Element_Node then
         raise Constraint_Error with "Not an element.";
      else
         begin
            return Node_Value (Get_Named_Item (Nodes.Attributes (Element),
                                               Name));
         exception
            when Constraint_Error =>
               raise Constraint_Error
                 with "Failed to extract """ & Name & """ attribute " &
                      "from """ & Node_Name (Element) & """ element.";
         end;
      end if;
   end Attribute;

   Input  : File_Input;
   Reader : Tree_Reader;
   Doc    : Document;

   Reception : Receptions.Dial_Plan.Instance;

   Dial_Plan_Title    : Ada.Strings.Unbounded.Unbounded_String;
   Start_Action_Title : Ada.Strings.Unbounded.Unbounded_String;
   End_Points         : Receptions.End_Point_Collection.Map;
   Decision_Trees     : Receptions.Decision_Tree_Collection.Map;
begin
   PBX.Start;

   Set_Public_ID (Input, Receptions.Dial_Plan.XML_Element_Name);

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

   Load_Dial_Plan :
   declare
      Dial_Plan : constant Node := Get_Element (Doc);
   begin
      Check (Element => Dial_Plan, Name => Receptions.Dial_Plan.XML_Element_Name);

      Dial_Plan_Title := Ada.Strings.Unbounded.To_Unbounded_String
                           (Attribute (Element => Dial_Plan,
                                       Name    => "title"));

      declare
         Start : Node := First_Child (Dial_Plan);
      begin
         loop
            exit when Start = null;
            exit when Node_Type (Start) = Element_Node;
            Start := Next_Sibling (Start);
         end loop;

         Check (Element => Start, Name => "start");

         Start_Action_Title := Ada.Strings.Unbounded.To_Unbounded_String
                                 (Attribute (Element => Start,
                                             Name    => "do"));
      end;

      Load_End_Points :
      declare
         End_Point : Node := First_Child (Dial_Plan);
      begin
         Find_End_Points :
         loop
            Next_End_Point :
            loop
               exit Find_End_Points when End_Point = null;
               exit Next_End_Point when Node_Type (End_Point) = Element_Node and then
                                        Node_Name (End_Point) = Receptions.End_Point.XML_Element_Name;
               End_Point := Next_Sibling (End_Point);
            end loop Next_End_Point;

            Check (Element => End_Point, Name => Receptions.End_Point.XML_Element_Name);

            declare
               package Busy_Signal renames Receptions.End_Points.Busy_Signal;
               package Hang_Up renames Receptions.End_Points.Hang_Up;
               package Interactive_Voice_Response
                 renames Receptions.End_Points.Interactive_Voice_Response;
               package Queue renames Receptions.End_Points.Queue;
               package Redirect renames Receptions.End_Points.Redirect;
               package Voice_Mail renames Receptions.End_Points.Voice_Mail;

               Title : constant String := Attribute (Element => End_Point,
                                                     Name    => "title");

               End_Point_Action : not null Node := First_Child (End_Point);
            begin
               loop
                  exit when End_Point_Action = null;
                  exit when Node_Type (End_Point_Action) = Element_Node;
                  End_Point_Action := Next_Sibling (End_Point_Action);
               end loop;

               if Node_Name (End_Point_Action) = Hang_Up.XML_Element_Name then
                  End_Points.Insert
                    (Key      => Title,
                     New_Item => Hang_Up.Create (Title => Title));
               elsif Node_Name (End_Point_Action) = Queue.XML_Element_Name then
                  declare
                     Q : constant Queue.Instance :=
                           Queue.Create
                             (Title => Title,
                              ID    => Attribute
                                         (Element => End_Point_Action,
                                          Name    => "id"));
                  begin
                     End_Points.Insert (Key      => Title,
                                        New_Item => Q);
                  end;
               elsif Node_Name (End_Point_Action) = Redirect.XML_Element_Name then
                  declare
                     R : constant Redirect.Instance :=
                           Redirect.Create
                             (Title => Title,
                              To    => Attribute
                                         (Element => End_Point_Action,
                                          Name    => "to"));
                  begin
                     End_Points.Insert (Key      => Title,
                                        New_Item => R);
                  end;
               elsif Node_Name (End_Point_Action) = Interactive_Voice_Response.XML_Element_Name then
                  End_Points.Insert
                    (Key      => Title,
                     New_Item => Interactive_Voice_Response.Create (Title => Title));
               elsif Node_Name (End_Point_Action) = Voice_Mail.XML_Element_Name then
                  declare
                     V : constant Voice_Mail.Instance :=
                           Voice_Mail.Create
                             (Title   => Title,
                              Play    => Attribute (Element => End_Point_Action,
                                                    Name    => "play"),
                              Send_To => Attribute (Element => End_Point_Action,
                                                    Name    => "send-to"));
                  begin
                     End_Points.Insert (Key      => Title,
                                        New_Item => V);
                  end;
               elsif Node_Name (End_Point_Action) = Busy_Signal.XML_Element_Name then
                  End_Points.Insert (Key      => Title,
                                     New_Item => Busy_Signal.Create (Title => Title));
               else
                  raise Constraint_Error
                    with "<" & Receptions.End_Point.XML_Element_Name &
                         "> element contains illegal element <" &
                         Node_Name (End_Point_Action) & ">.";
               end if;

               Ada.Text_IO.Put_Line (Item => "End-point type:        """ &
                                             Node_Name (End_Point_Action) &
                                             """");
               Ada.Text_IO.Put_Line (Item => "End-point title:       """ &
                                             Title & """");
            end;

            End_Point := Next_Sibling (End_Point);
         end loop Find_End_Points;
      end Load_End_Points;

      Load_Decision_Trees :
      declare
         Decision_Tree : Node := First_Child (Dial_Plan);
      begin
         Find_Decision_Trees :
         loop
            Next_Decision_Tree :
            loop
               exit Find_Decision_Trees
                 when Decision_Tree = null;
               exit Next_Decision_Tree
                 when Node_Type (Decision_Tree) = Element_Node and then
                      Node_Name (Decision_Tree) = Receptions.Decision_Tree.XML_Element_Name;
               Decision_Tree := Next_Sibling (Decision_Tree);
            end loop Next_Decision_Tree;

            Check (Element => Decision_Tree,
                   Name    => Receptions.Decision_Tree.XML_Element_Name);

            Load_Decision_Tree :
            declare
               Title : constant String := Attribute (Element => Decision_Tree,
                                                     Name    => "title");

               Child : not null Node := First_Child (Decision_Tree);
            begin
               Ada.Text_IO.Put_Line (Item => "Decision-tree title:   """ &
                                             Title & """");
            end Load_Decision_Tree;

            Decision_Tree := Next_Sibling (Decision_Tree);
         end loop Find_Decision_Trees;
      end Load_Decision_Trees;
   end Load_Dial_Plan;

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

   PBX.Stop;
exception
   when AMI.Response.Timeout =>
      null;
   when others =>
      PBX.Stop;
      raise;
end Load_Dial_Plan;
