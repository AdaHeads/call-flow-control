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

package body DOM.Support is
   function Attribute (Element : in     DOM.Core.Node;
                       Name    : in     String) return String is
      use DOM.Core, DOM.Core.Nodes;
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

   procedure Check (Element : in     DOM.Core.Node;
                    Name    : in     String) is
      use DOM.Core, DOM.Core.Nodes;
   begin
      if Element = null then
         raise Constraint_Error with "No element.";
      elsif Node_Type (Element) /= Element_Node then
         raise Constraint_Error with "Not an element.";
      elsif Node_Name (Element) /= Name then
         raise Constraint_Error with "Not an <" & Name & "> element.";
      end if;
   end Check;
end DOM.Support;
