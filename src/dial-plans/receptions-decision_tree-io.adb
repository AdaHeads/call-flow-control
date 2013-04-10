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

with DOM.Core.Nodes,
     DOM.Support;

with Receptions.Branch,
     Receptions.Branch.IO;

package body Receptions.Decision_Tree.IO is
   function Load (From : in DOM.Core.Node) return Instance is
      function Title return String;
      function Branches return Receptions.List_Of_Branches.Vector;
      function Fall_Back return String;

      function Branches return Receptions.List_Of_Branches.Vector is
         use DOM.Core, DOM.Core.Nodes, DOM.Support;
         Child       : Node := First_Child (From);
         Found, Done : Boolean;
      begin
         return Result : Receptions.List_Of_Branches.Vector do
            loop
               Find_First (Element => Child,
                           Name    => Receptions.Branch.XML_Element_Name,
                           Found   => Found);
               exit when not Found;

               Result.Append (Receptions.Branch.IO.Load (Child));

               Next (Element => Child,
                     Done    => Done);
               exit when Done;
            end loop;
         end return;
      end Branches;

      function Fall_Back return String is
         use DOM.Core, DOM.Support;
         Child : Node := Nodes.First_Child (From);
      begin
         Find_First (Element => Child,
                     Name    => "fall-back");
         return Attribute (Element => Child,
                           Name    => "do");
      end Fall_Back;

      function Title return String is
      begin
         return DOM.Support.Attribute (Element => From,
                                       Name    => "title");
      end Title;
   begin
      DOM.Support.Check (Element => From,
                         Name    => XML_Element_Name);
      return Create (Title     => Title,
                     Branches  => Branches,
                     Fall_Back => Fall_Back);
   end Load;
end Receptions.Decision_Tree.IO;
