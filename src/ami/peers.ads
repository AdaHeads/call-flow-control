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

with Ada.Calendar;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;
with Call_List;

package Peers is
   use Ada.Containers;
   use Ada.Strings.Unbounded;

   type SIP_Peer_Status_Type is (Unregistered, Registered);

   package Call_List is new
     Ada.Containers.Vectors (Index_Type   => Positive,
                             Element_Type => Call_List.Call_Type,
                             "="          => Call_List."=");

   type Peer_Type is
      record
         Agent_ID     : Unbounded_String;
         Defined      : Boolean := False;
         Status       : SIP_Peer_Status_Type := Unregistered;
         ChannelType  : Unbounded_String;
         Peer         : Unbounded_String;
         Port         : Unbounded_String;
         Address      : Unbounded_String;
         Paused       : Boolean := False;
         Last_Seen    : Ada.Calendar.Time := Ada.Calendar.Clock;
         Exten        : Unbounded_String;

         Computer_ID : Unbounded_String;
         --  Dete skal kun symbolere de informationen, der måtte komme senere
      end record;

   Null_Peer : Peer_Type :=
                 (Agent_ID     => Null_Unbounded_String,
                  Defined      => False,
                  Status       => Unregistered,
                  ChannelType  => Null_Unbounded_String,
                  Peer         => Null_Unbounded_String,
                  Port         => Null_Unbounded_String,
                  Address      => Null_Unbounded_String,
                  Paused       => False,
                  Last_Seen    => Ada.Calendar.Clock,
                  Exten        => Null_Unbounded_String,
                  Computer_ID  => Null_Unbounded_String);

   type Peer_Type_Access is access Peer_Type;

   function Hash (Peer_Address : in Unbounded_String) return Hash_Type;

   package Peer_List_Type is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Peer_Type,
      Hash            => Hash,
      Equivalent_Keys => "=");

   function Get_Peers_List return Peer_List_Type.Map;
   function Get_Exten (Peer : in Unbounded_String) return Unbounded_String;
   function Get_Peer_By_ID (Agent_ID : in Unbounded_String) return Peer_Type;
   function Get_Peer_By_PhoneName (PhoneName : in Unbounded_String)
                                   return Peer_Type;
   --     procedure Print_Peer (Peer : in Peer_Type);
   procedure Insert_Peer (New_Item : in Peer_Type);
   procedure Replace_Peer (Item : in Peer_Type);

   --  Debug
   function List_As_String return String;
end Peers;
