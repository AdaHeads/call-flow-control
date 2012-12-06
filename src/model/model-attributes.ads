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
with Model.Attribute;

package Model.Attributes is

   type List is tagged private;
   Null_List : constant List;

   procedure Add_Attribute
     (Instance  : in out List;
      Attribute : in     Model.Attribute.Object);
   --  Add an attribute object to the list.

   procedure For_Each
     (Instance : in List;
      Process  : not null access
        procedure (Element : in Model.Attribute.Object));
   --  For every contact attribute set added to Instance by Add_Attributes, an
   --  attributes object is handed to Process.

   function Get
     (ID : in Contact_Identifier)
      return List;
   --  Get the attributes list that belongs to ID.

   function To_JSON_Array
     (Instance : in List)
      return GNATCOLL.JSON.JSON_Array;
   --  Convert Instance to a JSON array. This call is convenient wrapper for
   --  the View.Attribute.To_JSON_Array function.

private

   function Equal_Elements
     (Left, Right : in Model.Attribute.Object)
      return Boolean;

   function Equivalent_Keys
     (Left, Right : in Attribute_ID)
      return Boolean;

   function Key_Hash
     (Key : in Attribute_ID)
      return Ada.Containers.Hash_Type;

   package Attributes_Map is new Ada.Containers.Hashed_Maps
     (Key_Type        => Attribute_ID,
      Element_Type    => Model.Attribute.Object,
      Hash            => Key_Hash,
      Equivalent_Keys => Equivalent_Keys,
      "="             => Equal_Elements);

   type List is tagged
      record
         Attributes : Attributes_Map.Map := Attributes_Map.Empty_Map;
      end record;

   Null_List : constant List := (Attributes => Attributes_Map.Empty_Map);

end Model.Attributes;
