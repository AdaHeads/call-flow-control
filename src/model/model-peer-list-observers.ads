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

with ESL.Packet;
with PBX.Observers;

package Model.Peer.List.Observers is
   procedure Register_Observers;
   --  Registers the appropriate observers.

   procedure Unregister_Observers;
   --  Unregisters the appropriate observers.

private
   type Peer_State_Observers is
     new PBX.Observers.Instance with
      record
         ID : Natural := 112001;
      end record;

   overriding
   procedure Notify (Observer : in Peer_State_Observers;
                     Packet   : in ESL.Packet.Instance);

   overriding
   function "=" (Left, Right : in Peer_State_Observers) return Boolean;

   type Reload_Config_Observers is
     new PBX.Observers.Instance with
      record
         ID : Natural := 112002;
      end record;

   overriding
   procedure Notify (Observer : in Reload_Config_Observers;
                     Packet   : in ESL.Packet.Instance);

   overriding
   function "=" (Left, Right : in Reload_Config_Observers) return Boolean;

end Model.Peer.List.Observers;
