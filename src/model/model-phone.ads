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

with Ada.Containers.Vectors;
with GNATCOLL.JSON;
with Ada.Strings.Unbounded;

package Model.Phone is
   use Model;
   use GNATCOLL.JSON;
   use Ada.Strings.Unbounded;

   Package_Name : constant String := "Model.Phone";

   Phone_ID_Key : constant String := "id";
   Kind_Key     : constant String := "kind";
   Value_Key    : constant String := "value";

   type Kinds is (Invalid, SIP, PSTN);

   type Instance is tagged
      record
         ID    : Phone_Identifier;
         Kind  : Kinds;
         Value : Unbounded_String;
      end record;

   subtype Extension is String;

   function Create_From_JSON (JSON : in JSON_Value) return Instance;
   --  Constructs a new instance, based on a JSON map.

--     function Fetch (Reception : in Reception_Identifier;
--                     Contact   : in Contact_Identifier;
--                     Phone     : in Phone_Identifier) return Instance;
   --  Fetches a given phone@contact@reception from a contact service.

   function Image (Object : Instance) return String;
   --  Returns a string representation of the instance.

      function "=" (Left, Right : in Instance) return Boolean;
   --  Equals operation. Two instances are considered equal if both their
   --  reception identifier and contact identifier are equal.

   No_Phone : constant Instance;
   --  No-object reference.

   Null_Extension : constant Extension;

   package Phone_Storage is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Instance);
   subtype List is Phone_Storage.Vector;

   function Create_From_JSON (JSON : in JSON_Array) return Model.Phone.List;

   function Image (Phones : List) return String;

private

   Null_Extension : constant Extension := "";

   No_Phone : constant Instance :=
     (ID    => 0,
      Kind  => Invalid,
      Value => To_Unbounded_String (Null_Extension));

end Model.Phone;
