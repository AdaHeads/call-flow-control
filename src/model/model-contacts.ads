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
with Common;
with GNATCOLL.SQL.Exec;
with Model.Contacts_Attributes;

package Model.Contacts is

   type Contact_Object is tagged private;
   Null_Contact_Object : constant Contact_Object;

   function Equal
     (Left, Right : in Model.Contacts_Attributes.Contact_Attributes_Object)
      return Boolean;
   --  Element equality function used by the Attributes_Map.

   package Attributes_Map is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Model.Contacts_Attributes.Contact_Attributes_Object,
      Hash            => Key_Hash,
      Equivalent_Keys => Equivalent_Keys,
      "="             => Equal);

   function Attributes
     (Contact : in Contact_Object)
      return Attributes_Map.Map;
   --  Return the a linked list of Contact_Attributes_Object's associated with
   --  Contact.

   function Contact_Id
     (Contact : in Contact_Object)
      return Contact_Identifier;
   --  Return the id of the Contact.

   function Full_Name
     (Contact : in Contact_Object)
      return String;
   --  Return the full name of the Contact.

   function Get
     (C_Id : in Contact_Identifier)
      return Contact_Object;
   --  Return the contact that match C_Id, complete with all the attributes
   --  that may be associated with the contact.

   function Get
     (C_Id : in Contact_Identifier;
      O_Id : in Organization_Identifier)
      return Contact_Object;
   --  Return the contact that match C_Id, complete with all the attributes
   --  that may be associated with the contact.

   procedure Get
     (C_Id    : in Contact_Identifier;
      Process : not null access
        procedure (Element : in Contact_Object'Class));
   --  For every contact with C_Id in the database, a Contact_Object is handed
   --  to Process.

   procedure Get
     (C_Id    : in Contact_Identifier;
      O_Id    : in Organization_Identifier;
      Process : not null access
        procedure (Element : in Contact_Object'Class));
   --  Hands a Contact_Object to Process for every contact in the database that
   --  match C_Id and belongs to O_Id.

   function Is_Human
     (Contact : in Contact_Object)
      return Boolean;
   --  Return whether or not the Contact is human.

   function To_JSON
     (Contact : in Contact_Object)
      return Common.JSON_String;
   --  Convert the Contact to a JSON string. This call is convenient wrapper
   --  for the View.Contact.To_JSON function.

private

   type Cursor is new GNATCOLL.SQL.Exec.Forward_Cursor with null record;

   type Contact_Object is tagged
      record
         Attr_Map  : Attributes_Map.Map;
         C_Id      : Contact_Identifier := 0;
         Full_Name : Unbounded_String := Null_Unbounded_String;
         Is_Human  : Boolean := True;
      end record;

   Null_Contact_Object : constant Contact_Object
     := (Attr_Map  => Attributes_Map.Empty_Map,
         C_Id      => 0,
         Full_Name => Null_Unbounded_String,
         Is_Human  => True);

   function Contact_Element
     (C : in out Cursor)
      return Contact_Object'Class;
   --  Transforms the low level index based Cursor into the more readable
   --  Contact_Object record.

end Model.Contacts;
