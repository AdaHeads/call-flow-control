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
with Common;
with Model.Organization;

package Model.Organizations is

   type Organization_List_Object is tagged private;
   Null_List : constant Organization_List_Object;

   type Data_Mode is (Mini, Midi);
   --  Mini: As plain as possible. No JSON document, no contacts.
   --  Midi: The organization JSON document is also fetched.

   procedure For_Each
     (Instance : in Organization_List_Object;
      Process  : not null access procedure
        (Element : in Model.Organization.Organization_Object));
   --  Hands a contact object to process for every contact found in Instance.

   function Get
     (Mode : in Data_Mode := Mini)
      return Organization_List_Object;
   --  Get a list of all organizations found in the database. The organization
   --  objects in the list does not contain any contacts.

   function To_JSON_String
     (Self : in out Organization_List_Object)
      return Common.JSON_String;
   --  Convert Self into a JSON string. This call is convenient wrapper
   --  for the View.Organization.To_JSON function.

private

   function Equal_Elements
     (Left, Right : in Model.Organization.Organization_Object)
      return Boolean;

   function Equivalent_Keys
     (Left, Right : in Organization_Identifier)
      return Boolean;

   function Key_Hash
     (Key : in Organization_Identifier)
      return Ada.Containers.Hash_Type;

   package Organization_Map is new Ada.Containers.Hashed_Maps
     (Key_Type        => Organization_Identifier,
      Element_Type    => Model.Organization.Organization_Object,
      Hash            => Key_Hash,
      Equivalent_Keys => Equivalent_Keys,
      "="             => Equal_Elements);

   type Organization_List_Object is tagged
      record
         Organizations : Organization_Map.Map := Organization_Map.Empty_Map;
      end record;

   Null_List : constant Organization_List_Object :=
                 (Organizations => Organization_Map.Empty_Map);

end Model.Organizations;
