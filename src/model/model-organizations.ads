-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                           Model.Organizations                             --
--                                                                           --
--                                  SPEC                                     --
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

with Ada.Containers.Hashed_Maps;
with Common;
with GNATCOLL.JSON;
with GNATCOLL.SQL.Exec;
with Model.Contacts;

package Model.Organizations is

   type Organization_Object is tagged private;
   Null_Organization_Object : constant Organization_Object;

   function Equal
     (Left, Right : in Model.Contacts.Contact_Object)
      return Boolean;
   --  Element equality function used by the Attributes_Map.

   package Contacts_Map is new Ada.Containers.Hashed_Maps
     (Key_Type        => Contact_Identifier,
      Element_Type    => Model.Contacts.Contact_Object,
      Hash            => Key_Hash,
      Equivalent_Keys => Equivalent_Keys,
      "="             => Equal);

   procedure Add_Contact
     (Organization : in out Organization_Object;
      Contact      : in     Model.Contacts.Contact_Object);
   --  Add a contact to Organization.

   function Contacts
     (Organization : in Organization_Object)
      return Contacts_Map.Map;
   --  Return all the contacts associated with Organization. Note that this
   --  map is only populated if one of the Get_Full methods has been used to
   --  fetch the organization.

   function Full_Name
     (Organization : in Organization_Object)
      return String;

   function Get_Basic
     (O_Id : in Organization_Identifier)
      return Organization_Object;
   --  Return the organization that match O_Id WITHOUT all the contacts.

   procedure Get_Basic
     (O_Id    : in Organization_Identifier;
      Process : not null access
        procedure (Element : in Organization_Object'Class));
   --  For every organization with O_Id in the database, an Organization_Object
   --  is handed to Process. These organization objects do NOT contain any
   --  contacts.

   function Get_Full
     (O_Id : in Organization_Identifier)
      return Organization_Object;
   --  Return the organization that match O_Id. This object contains all the
   --  contacts that are associated with the organization.

   procedure Get_Full
     (O_Id    : in Organization_Identifier;
      Process : not null access
        procedure (Element : in Organization_Object'Class));
   --  For every organization with O_Id in the database, an Organization_Object
   --  is handed to Process. Included in the Organization_Object is a

   function Identifier
     (Organization : in Organization_Object)
     return String;

   function JSON
     (Organization : in Organization_Object)
      return GNATCOLL.JSON.JSON_Value;

   function Organization_Id
     (Organization : in Organization_Object)
      return Organization_Identifier;

   function To_JSON
     (Organization : in Organization_Object)
      return Common.JSON_String;
   --  Convert the Contact to a JSON string. This call is convenient wrapper
   --  for the View.Contact.To_JSON function.

private

   type Cursor is new GNATCOLL.SQL.Exec.Forward_Cursor with null record;

   type Organization_Object is tagged
      record
         C_Map      : Contacts_Map.Map;
         Full_Name  : Unbounded_String := Null_Unbounded_String;
         Identifier : Unbounded_String := Null_Unbounded_String;
         JSON       : GNATCOLL.JSON.JSON_Value := GNATCOLL.JSON.JSON_Null;
         O_Id       : Organization_Identifier := 0;
      end record;

   Null_Organization_Object : constant Organization_Object
     := (C_Map      => Contacts_Map.Empty_Map,
         Full_Name  => Null_Unbounded_String,
         Identifier => Null_Unbounded_String,
         JSON       => GNATCOLL.JSON.JSON_Null,
         O_Id       => 0);

   function Organization_Element_Basic
     (C : in out Cursor)
      return Organization_Object'Class;
   --  Transforms the low level index based Cursor into the more readable
   --  Organization_Object record. This one does NOT contain any contacts.

      function Organization_Element_Full
     (C : in out Cursor)
      return Organization_Object'Class;
   --  Transforms the low level index based Cursor into the more readable
   --  Organization_Object record. This one DOES contain all contacts
   --  associated with the organization.

end Model.Organizations;
