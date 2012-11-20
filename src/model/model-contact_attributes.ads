-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                          Model.Contact_Attributes                         --
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

package Model.Contact_Attributes is

   type Contact_Attributes_Object is tagged private;
   Null_Contact_Attributes : constant Contact_Attributes_Object;

   type Contact_Attributes_List_Object is tagged private;
   Null_Contact_Attributes_List : constant
     Contact_Attributes_List_Object;

   procedure Add_Attributes
     (Self      : in out Contact_Attributes_List_Object;
      Attribute : in     Contact_Attributes_Object'Class);
   --  Add an attribute object to the list.

   function Create
     (Id   : in Attributes_Identifier;
      JSON : in GNATCOLL.JSON.JSON_Value)
      return Contact_Attributes_Object;
   --  Create a Contact_Attributes_Object.

   procedure For_Each
     (Self    : in Contact_Attributes_List_Object;
      Process : not null access
        procedure (Element : in Contact_Attributes_Object));
   --  TODO: Write comment

   procedure For_Each
     (C_Id    : in Contact_Identifier;
      Process : not null access
        procedure (Element : in Contact_Attributes_Object'Class));
   --  For every contact_attributes row with C_Id in the database, a
   --  Contact_Attributes_Object is handed to Process.

   procedure For_Each
     (Id      : in Attributes_Identifier;
      Process : not null access
        procedure (Element : in Contact_Attributes_Object'Class));
   --  For every contact_attributes row with Id in the database, a
   --  Contact_Attributes_Object is handed to Process.

   procedure Get
     (Self : in out Contact_Attributes_OBject;
      Id   : in     Attributes_Identifier);
   --  Get the contact attribute set that belongs to Id.

   function Contact_Id
     (Self : in Contact_Attributes_Object)
      return Contact_Identifier;
   --  Return the contact id for the Self attribute set.

   function JSON
     (Self : in Contact_Attributes_Object)
      return GNATCOLL.JSON.JSON_Value;
   --  Return the JSON document for the Self attribute set.

   function Organization_Id
     (Self : in Contact_Attributes_Object)
      return Organization_Identifier;
   --  Return the organization id for the Self Attribute set.

   function To_JSON
     (Self : in Contact_Attributes_Object)
      return Common.JSON_String;
   --  Convert Self to a JSON string. This call is convenient wrapper for the
   --  View.Contact_Attributes.To_JSON function.

private

   type Cursor is new GNATCOLL.SQL.Exec.Forward_Cursor with null record;

   function Equal_Elements
     (Left, Right : in Contact_Attributes_Object)
      return Boolean;

   function Equivalent_Keys
     (Left, Right : in Attributes_Identifier)
      return Boolean;
   --  Key equivalence function used by hashed maps.

   function Key_Hash
     (Key : in Attributes_Identifier)
      return Ada.Containers.Hash_Type;
   --  Hashing function used by the hashed maps.

   type Contact_Attributes_Object is tagged
      record
         Id   : Attributes_Identifier;
         JSON : GNATCOLL.JSON.JSON_Value := GNATCOLL.JSON.JSON_Null;
      end record;

   Null_Contact_Attributes : constant Contact_Attributes_Object :=
                               (Id   => (C_Id => 0, O_Id => 0),
                                JSON => GNATCOLL.JSON.JSON_Null);

   package Attributes_Map is new Ada.Containers.Hashed_Maps
     (Key_Type        => Attributes_Identifier,
      Element_Type    => Contact_Attributes_Object,
      Hash            => Key_Hash,
      Equivalent_Keys => Equivalent_Keys,
      "="             => Equal_Elements);

   type Contact_Attributes_List_Object is tagged
      record
         A_Map : Attributes_Map.Map := Attributes_Map.Empty_Map;
      end record;

   Null_Contact_Attributes_List : constant Contact_Attributes_List_Object
     := (A_Map => Attributes_Map.Empty_Map);

   function Contact_Attributes_Element
     (C : in out Cursor)
      return Contact_Attributes_Object'Class;
   --  Transforms the low level index based Cursor into the more readable
   --  Contact_Attributes_Object record.

end Model.Contact_Attributes;
