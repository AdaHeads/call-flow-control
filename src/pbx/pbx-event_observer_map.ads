-------------------------------------------------------------------------------
--                                                                           --
--                     Copyright (C) 2014-, AdaHeads K/S                     --
--                     Author: Kim Rostgaard Christensen                     --
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

with Ada.Containers.Indefinite_Vectors;
with ESL.Packet;
with Ada.Containers.Hashed_Maps;
with ESL.Packet_Keys;
with PBX.Observers;

package PBX.Event_Observer_Map is

   Package_Name : constant String := "PBX.Event_Observer_Map";

   type Instance is tagged private;

   procedure Register_Observer
     (Object   : in out Instance;
      Observer : in     PBX.Observers.Instance'Class);

   procedure Notify_Observers (Object : in Instance;
                               Packet : in ESL.Packet.Instance);

private
   use PBX.Observers;

   function Hash (Event_Key : ESL.Packet_Keys.Inbound_Events)
                  return Ada.Containers.Hash_Type;

   function Equivalent_Keys
     (Left, Right : ESL.Packet_Keys.Inbound_Events)
      return Boolean;

   package Observer_Storage is new
     Ada.Containers.Indefinite_Vectors
       (Index_Type => Natural,
        Element_Type => PBX.Observers.Instance'Class);
   subtype Observer_Lists is Observer_Storage.Vector;

   package Event_Observer_Storage is new
     Ada.Containers.Hashed_Maps (Key_Type => ESL.Packet_Keys.Inbound_Events,
                                 Element_Type => Observer_Lists,
                                 Hash => Hash,
                                 Equivalent_Keys => Equivalent_Keys,
                                 "=" => Observer_Storage."=");

   type Instance is tagged
      record
      Observer_List : Event_Observer_Storage.Map;
      end record;

end PBX.Event_Observer_Map;
