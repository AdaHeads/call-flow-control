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

with Receptions.Action,
     Receptions.Conditions.IO;

package body Receptions.Branch.IO is
   function Load (From : in DOM.Core.Node) return Instance is
      function Conditions return Receptions.Conditions.Instance;
      function Action return String;

      function Action return String is
         use DOM.Core, DOM.Support;
         Child : Node := Nodes.First_Child (From);
      begin
         Find_First (Element => Child,
                     Name    => Receptions.Action.XML_Element_Name);
         return Attribute (Element => Child,
                           Name    => "do");
      end Action;

      function Conditions return Receptions.Conditions.Instance is
         use DOM.Core, DOM.Support;
         Child : Node := Nodes.First_Child (From);
      begin
         Find_First (Element => Child,
                     Name    => Receptions.Conditions.XML_Element_Name);
         return Receptions.Conditions.IO.Load (From => Child);
      end Conditions;
   begin
      DOM.Support.Check (Element => From,
                         Name    => XML_Element_Name);
      return Create (Conditions => Conditions,
                     Action     => Action);
   end Load;
end Receptions.Branch.IO;
