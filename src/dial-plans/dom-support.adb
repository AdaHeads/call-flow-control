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

with System_Message.Debug;

package body DOM.Support is
   function Attribute (Element : in     DOM.Core.Node;
                       Name    : in     String) return String is
      use DOM.Core, DOM.Core.Nodes;
   begin
      System_Message.Debug.Looking_For_XML_Attribute (Message => Name);

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
      System_Message.Debug.Looking_For_XML_Element (Message => Name);

      if Element = null then
         raise Constraint_Error with "No element.";
      elsif Node_Type (Element) /= Element_Node then
         raise Constraint_Error with "Not an element.";
      elsif Node_Name (Element) /= Name then
         raise Constraint_Error with "Not an <" & Name & "> element.";
      end if;
   end Check;

   procedure Find_First (Element : in out DOM.Core.Node;
                         Name    : in     String) is
      use DOM.Core, DOM.Core.Nodes;
   begin
      System_Message.Debug.Looking_For_XML_Element (Message => Name);

      loop
         if Element = null then
            raise Constraint_Error
              with "The sequence does not contain a <" & Name & "> element.";
         elsif Node_Type (Element) = Element_Node and then
               Node_Name (Element) = Name then
            return;
         end if;

         Element := Next_Sibling (Element);
      end loop;
   end Find_First;

   procedure Find_First (Element : in out DOM.Core.Node;
                         Name    : in     String;
                         Found   :    out Boolean) is
      use DOM.Core, DOM.Core.Nodes;
   begin
      System_Message.Debug.Looking_For_XML_Element (Message => Name);

      loop
         if Element = null then
            Found := False;
            return;
         elsif Node_Type (Element) = Element_Node and then
               Node_Name (Element) = Name then
            Found := True;
            return;
         end if;

         Element := Next_Sibling (Element);
      end loop;
   end Find_First;

   procedure First (Element : in out DOM.Core.Node) is
      use DOM.Core, DOM.Core.Nodes;
   begin
      System_Message.Debug.Looking_For_XML_Element;

      loop
         if Element = null then
            raise Constraint_Error
              with "The sequence does not contain an XML element.";
         elsif Node_Type (Element) = Element_Node then
            return;
         end if;

         Element := Next_Sibling (Element);
      end loop;
   end First;

   procedure First (Element : in out DOM.Core.Node;
                    Found   :    out Boolean) is
      use DOM.Core, DOM.Core.Nodes;
   begin
      System_Message.Debug.Looking_For_XML_Element;

      loop
         if Element = null then
            Found := False;
            return;
         elsif Node_Type (Element) = Element_Node then
            Found := True;
            return;
         end if;

         Element := Next_Sibling (Element);
      end loop;
   end First;

   procedure Next (Element : in out DOM.Core.Node) is
   begin
      Element := DOM.Core.Nodes.Next_Sibling (Element);
   end Next;

   procedure Next (Element : in out DOM.Core.Node;
                   Done    :    out Boolean) is
      use type DOM.Core.Node;
   begin
      Element := DOM.Core.Nodes.Next_Sibling (Element);
      Done := (Element = null);
   end Next;
end DOM.Support;
