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

with DOM.Core.Nodes,
     DOM.Support;

with Receptions.Decision_Tree.IO,
     Receptions.End_Point.IO;

package body Receptions.Dial_Plan.IO is
   function Load (From : in DOM.Core.Node) return Instance is
      function Title return String;
      function Start_At return String;
      function End_Points return Receptions.End_Point_Collection.Map;
      function Decision_Trees return Receptions.Decision_Tree_Collection.Map;

      function Decision_Trees return Receptions.Decision_Tree_Collection.Map is
         Decision_Tree : DOM.Core.Node := DOM.Core.Nodes.First_Child (From);
         Found, Done   : Boolean;
      begin
         return Decision_Trees : Receptions.Decision_Tree_Collection.Map do
            Find_Decision_Trees :
            loop
               DOM.Support.Find_First
                 (Element => Decision_Tree,
                  Name    => Receptions.Decision_Tree.XML_Element_Name,
                  Found   => Found);
               exit Find_Decision_Trees when not Found;

               Decision_Trees.Insert
                 (Key      => DOM.Support.Attribute (Element => Decision_Tree,
                                                     Name    => "title"),
                  New_Item => Receptions.Decision_Tree.IO.Load
                                (From => Decision_Tree));

               DOM.Support.Next (Element => Decision_Tree,
                                 Done    => Done);
               exit Find_Decision_Trees when Done;
            end loop Find_Decision_Trees;
         end return;
      end Decision_Trees;

      function End_Points return Receptions.End_Point_Collection.Map is
         End_Point   : DOM.Core.Node := DOM.Core.Nodes.First_Child (From);
         Found, Done : Boolean;
      begin
         return End_Points : Receptions.End_Point_Collection.Map do
            Find_End_Points :
            loop
               DOM.Support.Find_First
                 (Element => End_Point,
                  Name    => Receptions.End_Point.XML_Element_Name,
                  Found   => Found);
               exit Find_End_Points when not Found;

               End_Points.Insert
                 (Key      => DOM.Support.Attribute (Element => End_Point,
                                                     Name    => "title"),
                  New_Item => Receptions.End_Point.IO.Load
                                (From => End_Point));

               DOM.Support.Next (Element => End_Point,
                                 Done    => Done);
               exit Find_End_Points when Done;
            end loop Find_End_Points;
         end return;
      end End_Points;

      function Start_At return String is
         Start : DOM.Core.Node := DOM.Core.Nodes.First_Child (From);
      begin
         DOM.Support.Find_First (Element => Start,
                                 Name    => "start");
         return DOM.Support.Attribute (Element => Start,
                                       Name    => "do");
      end Start_At;

      function Title return String is
      begin
         return DOM.Support.Attribute (Element => From,
                                       Name    => "title");
      end Title;
   begin
      DOM.Support.Check (Element => From,
                         Name    => XML_Element_Name);

      return Create (Title          => Title,
                     Start_At       => Start_At,
                     End_Points     => End_Points,
                     Decision_Trees => Decision_Trees);
   end Load;
end Receptions.Dial_Plan.IO;
