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

with Ada.Strings.Unbounded;
with Ada.Characters.Handling;

package body View.Peer is
   use Ada.Strings.Unbounded;
   use Ada.Characters.Handling;

   function To_JSON (Item : in Peer_Type)
                     return GNATCOLL.JSON.JSON_Value is
      use GNATCOLL.JSON;

      JSON      : constant JSON_Value := Create_Object;
      Peer_JSON : constant JSON_Value := Create_Object;
   begin
      Peer_JSON.Set_Field ("ID", Item.ID.To_String);
      Peer_JSON.Set_Field ("Agent_ID", Item.Agent_ID.To_String);
      Peer_JSON.Set_Field ("State", To_Lower (Item.State'Img));
      Peer_JSON.Set_Field ("Last_State", To_Lower (Item.Last_State'Img));
      Peer_JSON.Set_Field ("Port", To_String (Item.Port));
      Peer_JSON.Set_Field ("Address", To_String (Item.Address));
      Peer_JSON.Set_Field ("Last_Seen", AMI.Peer.To_String (Item.Last_Seen));
      JSON.Set_Field ("peer", Peer_JSON);

      return JSON;
   end To_JSON;

end View.Peer;
