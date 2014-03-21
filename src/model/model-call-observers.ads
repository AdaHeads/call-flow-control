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

with ESL.Packet;

with PBX.Observers;

package Model.Call.Observers is

   Package_Name : constant String := "Model.Call.Observers";

   procedure Register_Observers;
   --  Registers the appropriate observers.

   procedure Unregister_Observers;
   --  Unregisters the appropriate observers.
private

   type AdaHeads_Observer is
     new PBX.Observers.Instance with
      record
         ID : Natural := 111000;
      end record;
   --  Monitors for all events that are tagged AdaHeads::*"

   overriding
   procedure Notify (Observer : in AdaHeads_Observer;
                     Packet   : in ESL.Packet.Instance);

   overriding
   function "=" (Left, Right : in AdaHeads_Observer) return Boolean;

   type Bridge_Observer is
     new PBX.Observers.Instance with
      record
         ID : Natural := 111001;
      end record;

   overriding
   procedure Notify (Observer : in Bridge_Observer;
                     Packet   : in ESL.Packet.Instance);

   overriding
   function "=" (Left, Right : in Bridge_Observer) return Boolean;

   type Channel_Hold_Observer is
     new PBX.Observers.Instance with
      record
         ID : Natural := 111002;
      end record;

   overriding
   procedure Notify (Observer : in Channel_Hold_Observer;
                     Packet   : in ESL.Packet.Instance);

   overriding
   function "=" (Left, Right : in Channel_Hold_Observer) return Boolean;

   type Create_Observer is
     new PBX.Observers.Instance with
      record
         ID : Natural := 111003;
      end record;

   overriding
   procedure Notify (Observer : in Create_Observer;
                     Packet   : in ESL.Packet.Instance);

   overriding
   function "=" (Left, Right : in Create_Observer) return Boolean;

   type Destroy_Observer is
     new PBX.Observers.Instance with
      record
         ID : Natural := 111004;
      end record;

   overriding
   procedure Notify (Observer : in Destroy_Observer;
                     Packet   : in ESL.Packet.Instance);

   overriding
   function "=" (Left, Right : in Destroy_Observer) return Boolean;

   type Park_Observer is
     new PBX.Observers.Instance with
      record
         ID : Natural := 111005;
      end record;

   overriding
   procedure Notify (Observer : in Park_Observer;
                     Packet   : in ESL.Packet.Instance);

   overriding
   function "=" (Left, Right : in Park_Observer) return Boolean;

   type Channel_State_Observer is
     new PBX.Observers.Instance with
      record
         ID : Natural := 111006;
      end record;

   overriding
   procedure Notify (Observer : in Channel_State_Observer;
                     Packet   : in ESL.Packet.Instance);

   overriding
   function "=" (Left, Right : in Channel_State_Observer) return Boolean;

end Model.Call.Observers;
