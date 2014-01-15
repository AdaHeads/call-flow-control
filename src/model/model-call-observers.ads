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

with ESL.Client;
with ESL.Packet;
with ESL.Observer.Event_Observers;

package Model.Call.Observers is

   Package_Name : constant String := "PBX.Call.Event_Handers";

   --   procedure Register_Handlers;
   --  Registers the appropriate event handlers.
private
   type Newcall_Observer is
     new ESL.Observer.Event_Observers.Instance with null record;

   overriding
   procedure Notify (Observer : access Newcall_Observer;
                     Packet   : in     ESL.Packet.Instance;
                     Client   : in     ESL.Client.Reference);

   type Bridge_Observer is
     new ESL.Observer.Event_Observers.Instance with null record;

   overriding
   procedure Notify (Observer : access Bridge_Observer;
                     Packet   : in     ESL.Packet.Instance;
                     Client   : in     ESL.Client.Reference);

   type Channel_Hold_Observer is
     new ESL.Observer.Event_Observers.Instance with null record;

   overriding
   procedure Notify (Observer : access Channel_Hold_Observer;
                     Packet   : in     ESL.Packet.Instance;
                     Client   : in     ESL.Client.Reference);

   type Custom_Observer is
     new ESL.Observer.Event_Observers.Instance with null record;

   overriding
   procedure Notify (Observer : access Custom_Observer;
                     Packet   : in     ESL.Packet.Instance;
                     Client   : in     ESL.Client.Reference);

   type Destroy_Observer is
     new ESL.Observer.Event_Observers.Instance with null record;

   overriding
   procedure Notify (Observer : access Destroy_Observer;
                     Packet   : in     ESL.Packet.Instance;
                     Client   : in     ESL.Client.Reference);

   type Execute_Observer is
     new ESL.Observer.Event_Observers.Instance with null record;

   overriding
   procedure Notify (Observer : access Execute_Observer;
                     Packet   : in     ESL.Packet.Instance;
                     Client   : in     ESL.Client.Reference);

   type Park_Observer is
     new ESL.Observer.Event_Observers.Instance with null record;

   overriding
   procedure Notify (Observer : access Park_Observer;
                     Packet   : in     ESL.Packet.Instance;
                     Client   : in     ESL.Client.Reference);

end Model.Call.Observers;
