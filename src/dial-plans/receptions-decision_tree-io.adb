-------------------------------------------------------------------------------
--                                                                           --
--                      Copyright (C) 2013-, AdaHeads K/S                    --
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

with DOM.Core.Nodes;

package Receptions.Decision_Tree.IO is
   function Load (From : in DOM.Core.Node) return Instance is
   begin
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
                      Node_Name (Decision_Tree) =
                                     Receptions.Decision_Tree.XML_Element_Name;
               Decision_Tree := Next_Sibling (Decision_Tree);
            end loop Next_Decision_Tree;

            Check (Element => Decision_Tree,
                   Name    => Receptions.Decision_Tree.XML_Element_Name);

            Load_Decision_Tree :
            declare
               Title : constant String := Attribute (Element => Decision_Tree,
                                                     Name    => "title");

               Child : Node := First_Child (Decision_Tree);
            begin
               Ada.Text_IO.Put_Line (Item => "Decision-tree title:   """ &
                                             Title & """");

               Load_Branches :
               loop
                  Next_Branch_Or_Fallback :
                  loop
                     exit Load_Branches when Child = null;
                     exit Next_Branch_Or_Fallback
                       when Node_Type (Child) = Element_Node and then
                            Node_Name (Child) =
                                            Receptions.Branch.XML_Element_Name;
                     exit Load_Branches
                       when Node_Type (Child) = Element_Node and then
                            Node_Name (Child) = "fall-back";
                     Child := Next_Sibling (Child);
                  end loop Next_Branch_Or_Fallback;

                  Load_Branch :
                  declare
                     Branch : Node renames Child;
                  begin
                     Check (Element => Branch,
                            Name    => Receptions.Branch.XML_Element_Name);

                     raise Program_Error;
                  end Load_Branch;
               end loop Load_Branches;

               Load_Fallback :
               declare
                  Fallback : Node renames Child;
               begin
                  Check (Element => Fallback,
                         Name    => "fall-back");

                  raise Program_Error;
               end Load_Fallback;
            end Load_Decision_Tree;

            Decision_Tree := Next_Sibling (Decision_Tree);
         end loop Find_Decision_Trees;
      end Load_Decision_Trees;
   end Load;
end Receptions.Decision_Tree.IO;
