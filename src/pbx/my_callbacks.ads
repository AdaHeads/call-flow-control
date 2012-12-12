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
with AMI;
with AMI.Parser;

package My_Callbacks is
   use AMI.Parser;

   Package_Name : constant String := "My_Callbacks";

   Not_Implemented : exception;

   --  Event handlers
   procedure Peer_Status (Packet : in Packet_Type);
   procedure Core_Show_Channel (Packet : in Packet_Type);
   procedure Core_Show_Channels_Complete (Packet : in Packet_Type);
   procedure Default_Callback (Packet : in Packet_Type);

   procedure Dial          (Packet : in Packet_Type);
   procedure Hangup        (Packet : in Packet_Type);
   procedure Join          (Packet : in Packet_Type);
   procedure Leave         (Packet : in Packet_Type);
   procedure New_Channel   (Packet : in Packet_Type);
   procedure Queue_Abandon (Packet : in Packet_Type);
   --  procedure Unlink_Callback     (Event_List : in Event_List_Type.Map);

   procedure Peer_Entry (Packet : in Packet_Type);

   procedure Peer_List_Complete (Packet : in Packet_Type);
   procedure New_State (Packet : in Packet_Type);
   --  Update the state of a channel.

   procedure Bridge;
   procedure Agents;

end My_Callbacks;
