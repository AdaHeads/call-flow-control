-------------------------------------------------------------------------------
--                                                                           --
--                     Copyright (C) 2013-, AdaHeads K/S                     --
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

with Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded.Hash_Case_Insensitive;
with Ada.Strings.Unbounded.Equal_Case_Insensitive;

private with Ada.Strings.Unbounded;
with GNATCOLL.JSON;
package Model.User is
   use GNATCOLL.JSON;
   use Model;

   Package_Name : constant String := "Model.User";

   User_String       : constant String := "user";
   Identity_String   : constant String := "identity";
   Identities_String : constant String := "identities";

   type State is (Signed_Out, Idle, Paused, Away);

   type Name is new String
     with Dynamic_Predicate => (Name'Length > 0);

   type Identities is new String
     with Dynamic_Predicate => (Identities'Length > 0);

   type Identifications is new Natural;

   type Instance is tagged private;

   function Authenticated (Object : in Instance) return Boolean;

   function Create (ID     : in Identities;
                    Object : GNATCOLL.JSON.JSON_Value) return Instance;

   function "<" (Left, Right : in Instance) return Boolean;

   function "=" (Left, Right : in Instance) return Boolean;

   function "=" (Left, Right : in Identities) return Boolean;

   function Hash (Identity : Identities) return Ada.Containers.Hash_Type;

   function Hash (Identification : Identifications)
                  return Ada.Containers.Hash_Type;

   function Identity (Object : in Instance) return Identities;

   type Permission is (Receptionist, Service_Agent, Administrator);
   type Permission_List is array (Permission) of Boolean;

   No_Permissions : constant Permission_List := (others => False);

   function Permissions (User : in Instance) return Permission_List;

   function Value (Item : in String) return Identifications;

   function Value (Item : in String) return Identities;

   function To_JSON (Object : in Instance) return JSON_Value;

private
   use Ada.Strings.Unbounded;

   type Instance is tagged record
      ID         : Unbounded_String;
      Attributes : GNATCOLL.JSON.JSON_Value;
   end record;

   subtype Identity_Keys is Unbounded_String;

   function Key_Of (Item : Identities) return Unbounded_String;

   function Identity_Of (Item : Unbounded_String) return Identities;

   package User_Storage is new Ada.Containers.Hashed_Maps
     (Key_Type        => Identity_Keys,
      Element_Type    => User.Instance,
      Hash            => Ada.Strings.Unbounded.Hash_Case_Insensitive,
      Equivalent_Keys => Ada.Strings.Unbounded.Equal_Case_Insensitive);

   subtype User_Maps is  User_Storage.Map;

   package Lookup_Storage is new Ada.Containers.Hashed_Maps
     (Key_Type        => User.Identifications,
      Element_Type    => Identity_Keys,
      Hash            => User.Hash,
      Equivalent_Keys => User."=");

   subtype ID_Lookup_Maps is Lookup_Storage.Map;

end Model.User;
