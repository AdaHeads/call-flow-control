-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                  Peers                                    --
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
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;
with Model.Agent_ID;
with Model.Peer_ID;
with GNATCOLL.JSON;

with Common;
package Model.Peers is
   use Ada.Containers;
   use Ada.Strings.Unbounded;
   use Model.Agent_ID;
   use Model.Peer_ID;
   use Common;

   Peer_Not_Found : exception;
   --  When a peer is not found in the list, something is terribly wrong.
   --  It means we have an inconsistant state between Agent and Peer, and
   --  thus, we raise an exception.

   type SIP_Peer_Status_Type is (Unknown, Unregistered, Idle, Busy, Paused);

   type Peer_Type is tagged
      record
         ID           : Peer_ID_Type; --  Was 'peer'
         Agent_ID     : Agent_ID_Type;
         State        : SIP_Peer_Status_Type := Unregistered;
         Last_State   : SIP_Peer_Status_Type := Unknown;
         Port         : Unbounded_String;
         Address      : Unbounded_String;
         Last_Seen    : Time;
      end record;

   function Available (Peer : in Peer_Type) return Boolean;

   function To_JSON (Peer : in Peers.Peer_Type)
                     return GNATCOLL.JSON.JSON_Value;
   function To_String (Peer : in Peer_Type) return String;

   --  function Image (Item : in Peer_Type) return String;
   --     procedure Print_Peer (Peer : in Peer_Type);
   --  procedure Insert_Peer (New_Item : in Peer_Type);
   --  Inserts a new peer in the peer list, and overwrites existing peers with
   --  the same key.

   --  Debug
   --  function List_As_String return String;
   --  function Image return String;
   --  function Get_Peer_By_ID (Agent_ID : in Unbounded_String)
   --  return Peer_Type;
   --  function Get_Peer_By_PhoneName (PhoneName : in String)
   --                                 return Peer_Type;

   function Hash (Peer_ID : Peer_ID_Type) return Hash_Type;

   package Peer_List_Storage is new Ada.Containers.Hashed_Maps
     (Key_Type        => Peer_ID_Type,
      Element_Type    => Peer_Type,
      Hash            => Hash,
      Equivalent_Keys => "=");

   --  function Get_Peers_List return Peer_List_Storage.Map;
   --  function Get_Exten (Peer : in Unbounded_String) return Unbounded_String;

   protected type Peer_List_Type is
      function Get (Peer_ID : in Peer_ID_Type) return Peer_Type;
      function Contains (Peer_ID : in Peer_ID_Type) return Boolean;
      --        function Get_Peers_List return Peer_List_Storage.Map;
      --        function Get_Peer_By_ID (Agent_ID : in Unbounded_String)
      --                                 return Peer_Type;
      --        function Get_Peer_By_PhoneName (PhoneName : in String)
      --                                        return Peer_Type;
      procedure Insert (Peer : in Peer_Type);
      function To_String return String;
      function To_JSON return GNATCOLL.JSON.JSON_Value;
   private
      List : Peer_List_Storage.Map;
   end Peer_List_Type;

   Null_Peer : constant Peer_Type;

   List : Peer_List_Type;
private
      Null_Peer : constant Peer_Type :=
     (ID           => Null_Peer_ID,
      Agent_ID     => Null_Agent_ID,
      State        => Unregistered,
      Last_State   => Unknown,
      Port         => Null_Unbounded_String,
      Address      => Null_Unbounded_String,
      Last_Seen    => Current_Time);
end Model.Peers;
