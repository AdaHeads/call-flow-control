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

with GNATCOLL.JSON;

with Common;

package Model.Attribute is

   type Object is tagged private;
   Null_Object : constant Object;

   function Contact_ID
     (Instance : in Object)
      return Contact_Identifier;
   --  Return the contact identifier for Instance.

   function Create
     (ID   : in Attribute_Identifier;
      JSON : in GNATCOLL.JSON.JSON_Value)
      return Object;
   --  Create a contact attribute object. Note that the JSON is free-form. It
   --  must adhere to rules governed by the application that will ultimately
   --  use the JSON. No checks are done by Alice.

   function Get
     (ID : in Attribute_Identifier)
      return Object;
   --  Get the attribute object that belongs to ID.

   function ID
     (Instance : in Object)
      return Attribute_Identifier;
   --  Return the ID of Instance.

   function JSON
     (Instance : in Object)
      return GNATCOLL.JSON.JSON_Value;
   --  Return the JSON document for Instance.

   function Organization_ID
     (Instance : in Object)
      return Organization_Identifier;
   --  Return the organization identifier for Instance.

   function To_JSON_String
     (Instance : in Object)
      return Common.JSON_String;
   --  Convert Instance to a JSON string. This call is convenient wrapper for
   --  the View.Attribute.To_JSON_String function.

private

   type Object is tagged
      record
         ID   : Attribute_Identifier;
         JSON : GNATCOLL.JSON.JSON_Value := GNATCOLL.JSON.JSON_Null;
      end record;

   Null_Object : constant Object := (ID   => (CID => 0, OID => 0),
                                     JSON => GNATCOLL.JSON.JSON_Null);

end Model.Attribute;
