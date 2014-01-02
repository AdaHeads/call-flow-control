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

with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash_Case_Insensitive;
with Ada.Strings.Unbounded.Equal_Case_Insensitive;
with GNATCOLL.JSON;

with Common;

package Model.Peer is
   use Ada.Strings.Unbounded;
   use GNATCOLL.JSON;
   use Common;
   use Model;

   Package_Name : constant String := "Model.Peer";

   Contact_String    : constant String := "contact";
   Expires_String    : constant String := "expires";
   Extension_String  : constant String := "extension";
   Registered_String : constant String := "registered";
   --  Identifiers for the JSON map.

   subtype Identification is Unbounded_String;
   --  The identification is the peer ID corresponding to the user ID in
   --  FreeSWITCH (for instance 1000). This is typically, but not required
   --  to be, an integer.

   type Instance is tagged private;

   function Get_Identification (Object : in Instance) return String;

   function Create (User_ID : Identification;
                    Values  : JSON_Value) return Instance;
   --  Constructor which tries to detect the online status of the peer upon
   --  creation. Could potentially raise a constraint error.

   function To_JSON (Object : in Instance) return JSON_Value;
   --  Returns a JSON representation of the peer.

   procedure Bump_Expiry (Object     :    out Instance;
                          Time_Delta : in     Natural);
   --  Adds the Time_Delta to the expiry time of the peer.

   procedure Register (Object : out Instance);
   --  Mark the peer as registered.

   function Registered (Object : in Instance) return Boolean;
   --  Returns true if the peer is detected registered, false otherwise.

   procedure Unregister (Object : out Instance);
   --  Mark the peer as unregistered.

   Null_Identification : constant Identification;
private

   type Instance is tagged
      record
         User_ID     : Identification := Null_Identification;
         Values      : JSON_Value     := Create;
         Registered  : Boolean        := False;
         Expiry_Time : Common.Time    := Common.Null_Time;
      end record;

   Null_Identification : constant Identification := Null_Unbounded_String;

   package Peer_Storage is new Ada.Containers.Hashed_Maps
     (Key_Type        => Identification,
      Element_Type    => Peer.Instance,
      Hash            => Ada.Strings.Unbounded.Hash_Case_Insensitive,
      Equivalent_Keys => Ada.Strings.Unbounded.Equal_Case_Insensitive);

   subtype Peer_Maps is Peer_Storage.Map;

end Model.Peer;
