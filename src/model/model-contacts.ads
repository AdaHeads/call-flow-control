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

with Ada.Containers.Hashed_Maps;

with GNATCOLL.JSON;

with Model.Contact;

package Model.Contacts is

   type List is tagged private;
   Null_List : constant List;

   procedure Add_Contact
     (Instance : in out List;
      Contact  : in     Model.Contact.Object;
      ID       : in     Organization_Identifier);
   --  Add Contact to Instance.

   function Get
     (ID : in Organization_Identifier)
      return List;
   --  Get a list of contacts that belong to the ID organization.

   procedure For_Each
     (Instance : in List;
      Process  : not null access procedure
        (Element : in Model.Contact.Object));
   --  Hands a contact object to process for every contact found in Instance.

   function To_JSON_Array
     (Instance : in List)
      return GNATCOLL.JSON.JSON_Array;
   --  Convert Instance to a JSON array. This call is convenient wrapper for
   --  the View.Attribute.To_JSON_Array function.

private

   function Equal_Elements
     (Left, Right : in Model.Contact.Object)
      return Boolean;

   function Equivalent_Keys
     (Left, Right : in Organization_Contact_Identifier)
      return Boolean;

   function Key_Hash
     (Key : in Organization_Contact_Identifier)
      return Ada.Containers.Hash_Type;

   package Contact_Map is new Ada.Containers.Hashed_Maps
     (Key_Type        => Organization_Contact_Identifier,
      Element_Type    => Model.Contact.Object,
      Hash            => Key_Hash,
      Equivalent_Keys => Equivalent_Keys,
      "="             => Equal_Elements);

   type List is tagged
      record
         Contacts : Contact_Map.Map := Contact_Map.Empty_Map;
      end record;

   Null_List : constant List := (Contacts => Contact_Map.Empty_Map);

end Model.Contacts;
