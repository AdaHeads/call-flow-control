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

with ESL.Client;
with ESL.Packet;
with ESL.Observer.Event_Observers;

package Model.Peer.List.Observers is
   type Peer_State_Observers is
     new ESL.Observer.Event_Observers.Instance with null record;

   overriding
   procedure Notify (Observer : access Peer_State_Observers;
                     Packet   : in     ESL.Packet.Instance;
                     Client   : in     ESL.Client.Reference);

   type Reload_Config_Observers is
     new ESL.Observer.Event_Observers.Instance with null record;

   overriding
   procedure Notify (Observer : access Reload_Config_Observers;
                     Packet   : in     ESL.Packet.Instance;
                     Client   : in     ESL.Client.Reference);
end Model.Peer.List.Observers;
