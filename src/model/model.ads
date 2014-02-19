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

package Model is

   subtype Identifications is Natural;
   subtype Contact_Identifier is Identifications;
   subtype Organization_Identifier is Identifications;
   subtype Reception_Identifier is Identifications;
   subtype Phone_Identifier is Identifications;

   subtype Organization_URI is String;
--     with Dynamic_Predicate => (Organization_URI'Length <= 256);

   type Organization_Contact_Identifier is
      record
         Contact_ID      : Contact_Identifier := 0;
         Organization_ID : Organization_Identifier := 0;
      end record;
   --  Identifies the Contact_ID contact in the context of the
   --  Organization_ID organization.

   type Attribute_Identifier is new Organization_Contact_Identifier;
   --  Identifies a set of contact attributes for the Contact_ID
   --  contact in the context of the Organization_ID organization.

   Null_Organization_Identifier : constant Organization_Identifier;
   Null_Reception_Identifier    : constant Reception_Identifier;

private

   Null_Organization_Identifier : constant Organization_Identifier := 0;
   Null_Reception_Identifier    : constant Reception_Identifier    := 0;

end Model;
