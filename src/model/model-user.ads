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

with Ada.Containers,
     Ada.Containers.Hashed_Maps,
     Ada.Strings.Unbounded;

with Model.Peer;

with Handlers.Notifications;

with GNATCOLL.JSON;
package Model.User is
   use Ada.Strings.Unbounded;
   use GNATCOLL.JSON;
   use Model;

   Package_Name : constant String := "Model.User";

   ID_String         : constant String := "id";
   User_String       : constant String := "user";
   Name_String       : constant String := "name";
   Peer_String       : constant String := "peer";
   Extension_String  : constant String := "extension";
   Peer_ID_String    : constant String := Peer_String & "_id";
   Users_String      : constant String := User_String & "s";
   Groups_String     : constant String := "groups";
   Identity_String   : constant String := "identity";
   Identities_String : constant String := "identities";

   Parking_Lot_Prefix   : constant String := "park+";
   Receptionist_String  : constant String := "Receptionist";
   Administrator_String : constant String := "Administrator";
   Service_Agent_String : constant String := "Service agent";

   Call_URI_Prefix      : constant String := "user/";

   type States is (Unknown, Signed_Out, Idle, Paused, Away);

   type Name is new String;
--     with Dynamic_Predicate => (Name'Length > 0);

   subtype Identities is Unbounded_String;

   subtype Identifications is Natural;

   type Instance is tagged private;

   type Reference is access all Instance;

   function Authenticated (Object : in Instance) return Boolean;

   function Create (User_ID : in Identifications;
                    Object  : GNATCOLL.JSON.JSON_Value) return Instance;

   function Create (Object  : GNATCOLL.JSON.JSON_Value) return Instance;

   function Create (User_ID : in Identifications;
                    Object  : GNATCOLL.JSON.JSON_Value) return Reference;

   function "<" (Left, Right : in Instance) return Boolean;

   function "=" (Left, Right : in Instance) return Boolean;

   function "=" (Left, Right : in Identities) return Boolean;

   function Image (Object : in Instance) return String;

   function Identification (Object : in Instance) return Identifications;

   function Parking_Lot_Identifier (Object : in Instance) return String;

   function Image (Identification : Identifications) return String;

   function Image (Identity : Identities) return String;

   type Permission is (Receptionist, Service_Agent, Administrator);
   type Permission_List is array (Permission) of Boolean;

   No_Permissions : constant Permission_List := (others => False);

   function Permissions (User : in Instance) return Permission_List;

   function WebSocket (User : in Instance) return
     Handlers.Notifications.Object;

   function Value (Item : in String) return Identifications;

   function Value (Item : in String) return Identities;

   function To_JSON (Object : in Instance) return JSON_Value;

   procedure Change_State (Object    :    out Instance;
                           New_State : in     States);

   function Current_State (Object : in Instance) return States;

   function Peer (Object : in Instance) return Model.Peer.Instance;
   --  Returns the peer currently associated with the user.

   function Call_URI (Object : in Instance) return String;

   function No_User return Instance;
   pragma Inline (No_User);

   Null_User     : constant Reference;
   Null_Identity : constant Identities;
   Null_Identification : constant Identifications;
private
   package Peers renames Model.Peer;

   Null_Identification : constant Identifications := 0;
   Null_User           : constant Reference       := null;
   Null_Identity       : constant Identities      := Null_Unbounded_String;

   type Instance is tagged record
      ID            : Identifications            := Null_Identification;
      WebSocket     : Handlers.Notifications.Object;
      Current_State : States                     := Unknown;
      Peer          : Model.Peer.Identification  := Peers.Null_Identification;
      Attributes    : GNATCOLL.JSON.JSON_Value   := Create;
   end record;

   subtype Identity_Keys is Unbounded_String;

   function Key_Of (Item : Identities) return Unbounded_String;

   function Identity_Of (Item : Unbounded_String) return Identities;

   function Hash (Identity : Identities) return Ada.Containers.Hash_Type;

   function Hash (Identification : Identifications)
                  return Ada.Containers.Hash_Type;

   package User_Storage is new Ada.Containers.Hashed_Maps
     (Key_Type        => Identifications,
      Element_Type    => User.Instance,
      Hash            => Hash,
      Equivalent_Keys => "=");

   subtype User_Maps is  User_Storage.Map;

   package Lookup_Storage is new Ada.Containers.Hashed_Maps
     (Key_Type        => User.Identities,
      Element_Type    => User.Identifications,
      Hash            => Hash,
      Equivalent_Keys => "=");

   subtype Identity_Maps is Lookup_Storage.Map;

end Model.User;
