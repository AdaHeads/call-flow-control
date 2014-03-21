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

with System_Messages;

package body PBX.Event_Observer_Map is

   use System_Messages;

   -----------------------
   --  Equivalent_Keys  --
   -----------------------

   function Equivalent_Keys (Left, Right : ESL.Packet_Keys.Inbound_Events)
                          return Boolean is
      use Ada.Containers;
      use ESL.Packet_Keys;
   begin
      return Left = Right;
   end Equivalent_Keys;

   ------------
   --  Hash  --
   ------------

   function Hash (Event_Key : ESL.Packet_Keys.Inbound_Events)
                  return Ada.Containers.Hash_Type is
   begin
      return ESL.Packet_Keys.Inbound_Events'Pos (Event_Key);
   end Hash;

   ------------------------
   --  Notify_Observers  --
   ------------------------

   procedure Notify_Observers (Object : in Instance;
                               Packet : in ESL.Packet.Instance) is
      use Observer_Storage;
   begin
      if Packet.Is_Event then
         if Object.Observer_List.Contains (Packet.Event) then
            declare
               Observers_To_Notify : Observer_Lists renames
                 Object.Observer_List.Element (Packet.Event);
            begin
               for Cursor in Observers_To_Notify.Iterate loop
                  Element (Cursor).Notify (Packet);
               end loop;
            end;
         end if;
      end if;
   end Notify_Observers;

   -------------------------
   --  Register_Observer  --
   -------------------------

   procedure Register_Observer
     (Object   : in out Instance;
      Observer : in     PBX.Observers.Instance'Class) is

      Context : constant String := Package_Name & ".Register_Observer";

      procedure Append_To_List
        (Key  : in     ESL.Packet_Keys.Inbound_Events;
         List : in out Observer_Lists);

      procedure Append_To_List
        (Key  : in     ESL.Packet_Keys.Inbound_Events;
         List : in out Observer_Lists) is
         pragma Unreferenced (Key);
      begin
         if not List.Contains (Observer) then
            List.Append (Observer);
         end if;
      end Append_To_List;

   begin
      if not Object.Observer_List.Contains (Observer.Observing_Event) then
         System_Messages.Debug (Message => "Registering observer for event: " &
                                  Observer.Observing_Event'Img &
                                  " in new list",
                                Context => Context);
         declare
            New_List : Observer_Lists;
         begin
            New_List.Append (New_Item => Observer);
            Object.Observer_List.Insert
              (Key      => Observer.Observing_Event,
               New_Item => New_List);
         end;
      else
         System_Messages.Debug (Message => "Registering observer for event: " &
                                  Observer.Observing_Event'Img &
                                  " in existing list",
                                Context => Context);
         Object.Observer_List.Update_Element
           (Position => Object.Observer_List.Find (Observer.Observing_Event),
            Process  => Append_To_List'Access);
      end if;
   end Register_Observer;

end PBX.Event_Observer_Map;
