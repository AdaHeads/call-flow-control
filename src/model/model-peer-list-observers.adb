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

with ESL.Packet_Keys;

with PBX,
     PBX.Action,
     PBX.Event_Stream,
     PBX.Magic_Constants;

with System_Messages;

package body Model.Peer.List.Observers is
   use ESL.Packet_Keys;

   function "=" (Left, Right : in Peer_State_Observers) return Boolean is
   begin
      return
        Left.ID              = Right.ID and
        Left.Observing_Event = Right.Observing_Event;
   end "=";

   function "=" (Left, Right : in Reload_Config_Observers) return Boolean is
   begin
      return
        Left.ID              = Right.ID and
        Left.Observing_Event = Right.Observing_Event;
   end "=";

   ---------------------------
   --  Peer_State Observer  --
   ---------------------------

   overriding
   procedure Notify (Observer : in Peer_State_Observers;
                     Packet   : in ESL.Packet.Instance) is
      use ESL;

      pragma Unreferenced (Observer);

      package Constants renames PBX.Magic_Constants;

      Context   : constant String      :=
        Package_Name & ".Notify (Peer_State observer)";
      pragma Unreferenced (Context);

      Subevent : String renames Packet.Field (Event_Subclass).Decoded_Value;

   begin
      if Subevent = Constants.Sofia_Register then
         declare
            ID : constant Model.Peer.Identification :=
              To_Unbounded_String
                (Packet.Field (Key => Username).Decoded_Value);
            Expire_Delta : Natural := 0;
         begin

            Expire_Delta :=
              Natural'Value (Packet.Field (Key => Expires).Decoded_Value);

            Model.Peer.List.Get_Singleton.Register
              (Identity => ID,
               Contact  => Packet.Field (Key => Contact).Decoded_Value,
               Expiry   => Expire_Delta);

         end;
      elsif Subevent = Constants.Sofia_Unregister then
         declare
            ID : constant Model.Peer.Identification :=
              To_Unbounded_String
                (Packet.Field (Key => Username).Decoded_Value);
         begin
            Model.Peer.List.Get_Singleton.Unregister (Identity => ID);
         end;
      end if;
   end Notify;

   ------------------------------
   --  Config reload Observer  --
   ------------------------------

   overriding
   procedure Notify (Observer : in Reload_Config_Observers;
                     Packet   : in ESL.Packet.Instance) is
      use ESL;

      pragma Unreferenced (Observer);

      Context   : constant String      :=
        Package_Name & ".Notify (Config reload observer)";
   begin
      System_Messages.Information
        (Context => Context,
         Message => "Reloading peer list in reponse to " &
           Packet.Event'Img & " event.");
      PBX.Action.Update_SIP_Peer_List;
   end Notify;

   --------------------------
   --  Register_Observers  --
   --------------------------

   procedure Register_Observers is
   begin
      System_Messages.Information
        (Context => Package_Name,
         Message => "Registering observers.");

      PBX.Event_Stream.Observer_Map.Register_Observer
        (Observer => Peer_State_Observers'
           (Observing_Event => ESL.Packet_Keys.CUSTOM,
            ID              => <>));

      PBX.Event_Stream.Observer_Map.Register_Observer
        (Observer => Reload_Config_Observers'
           (Observing_Event => ESL.Packet_Keys.RELOADXML,
            ID              => <>));

      --        Observer_List.Append
--          (New_Item => new Peer_State_Observers
--             (Observing => ESL.Client.Tasking.Event_Stream
--                (Client => PBX.Client,
--                 Stream => ESL.Packet_Keys.CUSTOM)));
--        Observer_List.Append
--          (New_Item => new Reload_Config_Observers
--             (Observing => ESL.Client.Tasking.Event_Stream
--                (Client => PBX.Client,
--                 Stream => ESL.Packet_Keys.RELOADXML)));
   end Register_Observers;

   ----------------------------
   --  Unregister_Observers  --
   ----------------------------

   procedure Unregister_Observers is
   begin
      System_Messages.Information
        (Context => Package_Name,
         Message => "Unregistering observers.");
--        for Item of Observer_List loop
--           Item.Finalize;
--        end loop;
--      Observer_List.Clear;
   end Unregister_Observers;

end Model.Peer.List.Observers;
