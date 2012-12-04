-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                             Model.Contacts                                --
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
with Ada.Strings.Unbounded;
with Common;
with Model.Contact_Attributes;

package Model.Contacts is

   type Contact_Object is tagged private;
   Null_Contact : constant Contact_Object;

   type Contact_List_Object is tagged private;
   Null_Contact_List : constant Contact_List_Object;

   procedure Add_Contact
     (Self    : in out Contact_List_Object;
      Contact : in     Contact_Object'Class;
      O_ID    : in     Organization_Identifier);
   --  Add Contact to the Self Contact_List_Object.

   procedure Add_Attribute
     (Self      : in out Contact_Object;
      Attribute : in     Model.Contact_Attributes.Contact_Attributes_Object);
   --  Add Attribute to the Self Contact_Object.

   function Attributes
     (Self : in Contact_Object)
      return Model.Contact_Attributes.Contact_Attributes_List_Object;
   --  Return the Contact_Attributes_Object's associated with Self.

   function Contact_ID
     (Self : in Contact_Object)
      return Contact_Identifier;
   --  Return the ID of the Contact.

   function Create
     (C_ID      : in Contact_Identifier;
      Full_Name : in String;
      Is_Human  : in Boolean)
      return Contact_Object;
   --  Create a Contact_Object.

   function Full_Name
     (Self : in Contact_Object)
      return String;
   --  Return the full name of the Contact.

   function Get
     (ID : in Contact_Identifier)
      return Contact_Object;
   --  Get the contact that match C_ID, complete with all the attributes that
   --  may be associated with the contact.

   procedure Get
     (Self : in out Contact_List_Object;
      O_ID : in     Organization_Identifier);
   --  Get a list of contacts that belong to the O_ID organization.

   function Get
     (ID : in Organization_Contact_Identifier)
      return Contact_Object;
   --  Return the contact that match ID, complete with all the attributes
   --  that may be associated with the contact.

   procedure For_Each
     (C_ID    : in Contact_Identifier;
      Process : not null access
        procedure (Element : in Contact_Object));
   --  For every contact with C_ID in the database, a Contact_Object is handed
   --  to Process.

   procedure For_Each
     (O_ID    : in Organization_Identifier;
      Process : not null access
        procedure (Element : in Contact_Object));
   --  Hands a Contact_Object to Process for every contact in the database that
   --  belongs to O_ID.

   procedure For_Each
     (C_ID    : in Contact_Identifier;
      O_ID    : in Organization_Identifier;
      Process : not null access
        procedure (Element : in Contact_Object));
   --  Hands a Contact_Object to Process for every contact in the database that
   --  match C_ID and belongs to O_ID.

   procedure For_Each
     (Self    : in Contact_List_Object;
      Process : not null access
        procedure (Element : in Contact_Object));
   --  Hands a Contact_Object to process for every contact found in the
   --  Contact_List_Object.

   function Is_Human
     (Self : in Contact_Object)
      return Boolean;
   --  Return whether or not the Self contact is human.

   function To_JSON
     (Self : in Contact_Object)
      return Common.JSON_String;
   --  Convert the Self contact to a JSON string. This call is convenient
   --  wrapper for the View.Contact.To_JSON function.

private

   use Ada.Strings.Unbounded;

   function Equal_Elements
     (Left, Right : in Contact_Object)
      return Boolean;

   function Equivalent_Keys
     (Left, Right : in Organization_Contact_Identifier)
      return Boolean;
   --  Key equivalence function used by hashed maps.

   function Key_Hash
     (Key : in Organization_Contact_Identifier)
      return Ada.Containers.Hash_Type;
   --  Hashing function used by the hashed maps.

   type Contact_Object is tagged
      record
         Attr_List : Model.Contact_Attributes.Contact_Attributes_List_Object;
         C_ID      : Contact_Identifier := 0;
         Full_Name : Unbounded_String := Null_Unbounded_String;
         Is_Human  : Boolean := True;
      end record;

   Null_Contact : constant Contact_Object
     := (Attr_List => Model.Contact_Attributes.Null_Contact_Attributes_List,
         C_ID      => 0,
         Full_Name => Null_Unbounded_String,
         Is_Human  => True);

   package Contact_Map is new Ada.Containers.Hashed_Maps
     (Key_Type        => Organization_Contact_Identifier,
      Element_Type    => Contact_Object,
      Hash            => Key_Hash,
      Equivalent_Keys => Equivalent_Keys,
      "="             => Equal_Elements);

   type Contact_List_Object is tagged
      record
         Contacts : Contact_Map.Map := Contact_Map.Empty_Map;
      end record;

   Null_Contact_List : constant Contact_List_Object
     := (Contacts => Contact_Map.Empty_Map);

end Model.Contacts;
