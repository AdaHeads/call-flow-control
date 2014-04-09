-------------------------------------------------------------------------------
--                                                                           --
--                     Copyright (C) 2014-, AdaHeads K/S                     --
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

with
  GNATCOLL.JSON;
with
  Model.Phone,
  Model.Token;

private
with
  Ada.Containers.Vectors,
  Ada.Strings.Unbounded;
private
with
  Ada_2012.Strings.Unbounded.Equal_Case_Insensitive;

package Model.Contact is
   use GNATCOLL.JSON;

   Package_Name : constant String := "Model.Contact";

   Contact_ID_Key        : constant String := "contact_id";
   Reception_ID_Key      : constant String := "reception_id";
   Phones_Key            : constant String := "phones";

   type Instance is tagged private;

   function Create_From_JSON (JSON : in JSON_Value) return Instance;
   --  Constructs a new instance, based on a JSON map.

   function Fetch (Reception  : in Reception_Identifier;
                   Contact    : in Contact_Identifier;
                   Auth_Token : in Token.Instance) return Instance;
   --  Fetches a given contact@reception from a contact service.

   function Extension_Of (Object   : Instance;
                          Phone_ID : Phone_Identifier) return String;
   --  Returns the specific extension assciated with the Phone_Identifer.

   function Image (Object : Instance) return String;
   --  Returns a string representation of the instance.

   overriding
   function "=" (Left, Right : in Instance) return Boolean;
   --  Equals operation. Two instances are considered equal if both their
   --  reception identifier and contact identifier are equal.

   No_Contact : constant Instance;
   --  No-object reference.

private
   use Ada.Strings.Unbounded;

   package Phones_Storage is new Ada.Containers.Vectors
     (Index_Type   => Phone_Identifier,
      Element_Type => Unbounded_String,
      "="          => Ada_2012.Strings.Unbounded.Equal_Case_Insensitive);

   subtype Phone_List is Phones_Storage.Vector;

   type Instance is tagged
      record
         Contact_ID   : Contact_Identifier;
         Reception_ID : Reception_Identifier;
         Phones       : Model.Phone.List;
      end record;

   No_Contact : constant Instance :=
     (Contact_ID   => 0,
      Reception_ID => Model.Null_Reception_Identifier,
      Phones       => Model.Phone.Phone_Storage.Empty_Vector);

end Model.Contact;
