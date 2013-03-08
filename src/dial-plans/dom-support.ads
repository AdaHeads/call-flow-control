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

with DOM.Core;

package DOM.Support is
   procedure Check (Element : in     DOM.Core.Node;
                    Name    : in     String);
   --  Check if <Element> is an XML element named <Name>.
   --  Will raise Constraint_Error with a descriptive message if it is not the
   --  case.

   function Attribute (Element : in     DOM.Core.Node;
                       Name    : in     String) return String;
   --  Get the value of the attribute <Name> of the XML element <Element>.
   --  Will raise Constraint_Error with a descriptive message if <Element> is
   --  not an XML element as well as if <Element> does not have an attribute
   --  named <Name>.

   procedure Find_First (Element : in out DOM.Core.Node;
                         Name    : in     String);
   --  Find the first XML element named <Name> in a sequence of sibling
   --  elements starting at <Element>.
   --  Will raise Constraint_Error with a descriptive message if the sequence
   --  does not contain a matching element.

   procedure Find_First (Element : in out DOM.Core.Node;
                         Name    : in     String;
                         Found   :    out Boolean);
   --  Find the first XML element named <Name> in a sequence of sibling
   --  elements starting at <Element>.  <Found> is true if a matching element
   --  is found, and false if no matching element is found in the sequence.

   procedure Next (Element : in out DOM.Core.Node);
   --  Go to the next XML element in a sequence of sibling elements starting
   --  at <Element>.
   --  Will raise Constraint_Error with a descriptive message if the end of
   --  the sequence is reached.

   procedure Next (Element : in out DOM.Core.Node;
                   Done    :    out Boolean);
   --  Go to the next XML element in a sequence of sibling elements starting
   --  at <Element>.  <Done> is set to False if the end of the sequence is
   --  reached.
end DOM.Support;
