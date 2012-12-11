-------------------------------------------------------------------------------
--                                                                           --
--                     Copyright (C) 2012-, AdaHeads K/S                     --
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

with Ada.Strings.Bounded;
with Ada.Containers.Doubly_Linked_Lists;

with AMI.Parser;

package AMI.Packet.Field is
   package Key_String is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max => 64);

   package Value_String is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max => 256);

   --  TODO: Make private
   type Field is tagged record
      Key   : Key_String.Bounded_String;
      Value : Value_String.Bounded_String;
   end record;

   function Create (Key :   in AMI.Parser.AMI_Key_Type;
                    Value : in String) return Field;

   function Create (Key :   in String;
                    Value : in String) return Field;
   --  Constructors.

   function To_AMI_Line (F : in Field) return AMI_Line;

   package Field_List is new Ada.Containers.Doubly_Linked_Lists (Field);

end AMI.Packet.Field;
