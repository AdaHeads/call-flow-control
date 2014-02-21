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

with Ada.Containers.Vectors;

with PBX.Magic_Constants;
with ESL.Packet_Keys;
with ESL.Client.Tasking;
with System_Messages;

package body Model.Call.Observers is
   use ESL.Packet_Keys;

   package Constants renames PBX.Magic_Constants;

   type Observer_Reference is access all
     ESL.Observer.Event_Observers.Instance'Class;

   package Observer_Storgage is new
     Ada.Containers.Vectors (Index_Type => Natural,
                             Element_Type => Observer_Reference);
   subtype Observer_Lists is Observer_Storgage.Vector;

   Observer_List : Observer_Lists;

   procedure Create_Call (From : in ESL.Packet.Instance);

   -------------------
   --  Create_Call  --
   -------------------

   procedure Create_Call (From : in ESL.Packet.Instance) is

      --  Context   : constant String      :=
      --   Package_Name & ".Create_Call";

      Packet : ESL.Packet.Instance renames From;

      ID  : Identification renames
        Value (Packet.Field (Unique_ID).Value);

      Direction : String renames
        Packet.Field (Key => Call_Direction).Decoded_Value;

      From_Extension : String renames
        Packet.Field (Key => Caller_Caller_ID_Number).Decoded_Value;

      Extension : String renames
        Packet.Field (Key => Caller_Destination_Number).Decoded_Value;

      Inbound : Boolean := True;
   begin
      if Direction /= "inbound" then
         Inbound := False;
      end if;

      Create_And_Insert
        (Inbound         => Inbound,
         ID              => ID,
         Reception_ID    => Null_Reception_Identifier,
         Extension       => Extension,
         From_Extension  => From_Extension);
   end Create_Call;

   -------------------------
   --  AdaHeads observer  --
   -------------------------

   procedure Notify (Observer : access AdaHeads_Observer;
                     Packet   : in     ESL.Packet.Instance;
                     Client   : in     ESL.Client.Reference) is
      pragma Unreferenced (Observer, Client);

      Context : constant String :=
        Package_Name & ".Notify (AdaHeads Subclass Observer)";
   begin
      if Packet.Subevent = Constants.Prequeue_Enter then
         Get (Packet.UUID).Set_Reception_ID
           (Reception_Identifier'Value
              (Packet.Variables.Get
                 (Key     => Constants.Reception_ID,
                  Default => Null_Reception_Identifier'Img)));
         Get (Packet.UUID).Change_State (New_State => Ringing);

      elsif Packet.Subevent = Constants.Outbound_Call then
         --  TODO: Tag the call with the reception.
         Get (Packet.UUID).Change_State (New_State => Ringing);

      elsif Packet.Subevent = Constants.Prequeue_Leave then
         Get (Packet.UUID).Change_State (New_State => Transferring);
         Get (Packet.UUID).Lock;
      elsif Packet.Subevent = Constants.Waitqueue_Enter then
         Get (Packet.UUID).Unlock;
         Get (Packet.UUID).Change_State (New_State => Queued);
      elsif Packet.Subevent = Constants.Parking_Lot_Enter then
         Get (Packet.UUID).Change_State (New_State => Parked);
      elsif Packet.Subevent = Constants.Parking_Lot_Leave then
         Get (Packet.UUID).Change_State (New_State => Transferring);
      end if;
   exception
      when Event : others =>
         System_Messages.Critical_Exception
           (Message => "Unhandled exception",
            Event   => Event,
            Context => Context);
   end Notify;

   --------------
   --  Bridge  --
   --------------

   overriding
   procedure Notify (Observer : access Bridge_Observer;
                     Packet   : in     ESL.Packet.Instance;
                     Client   : in     ESL.Client.Reference) is
      pragma Unreferenced (Observer);
      pragma Unreferenced (Client);

      Context   : constant String      :=
        Package_Name & ".Notify (Bridge_Observer)";
      ID_A  : Identification renames
        Value (Packet.Field (Bridge_A_Unique_ID).Value);
      ID_B  : Identification renames
        Value (Packet.Field (Bridge_B_Unique_ID).Value);
   begin

      Call.Link (ID_1 => ID_A,
                 ID_2 => ID_B);
      Get (Packet.UUID).Change_State (New_State => Speaking);

   exception
      when Event : others =>
         System_Messages.Critical_Exception
           (Message => "Unhandled exception",
            Event   => Event,
            Context => Context);

   end Notify;

   --------------------
   --  Channel_Hold  --
   --------------------

   overriding
   procedure Notify (Observer : access Channel_Hold_Observer;
                     Packet   : in     ESL.Packet.Instance;
                     Client   : in     ESL.Client.Reference) is
      pragma Unreferenced (Observer);
      pragma Unreferenced (Client);
      Context   : constant String      :=
        Package_Name & ".Notify (Channel_Hold_Observer)";

      ID  : Identification renames
        Value (Packet.Field (Unique_ID).Value);
   begin

      Get (ID).Change_State (New_State => Parked);

   exception
      when Event : others =>
         System_Messages.Critical_Exception
           (Message => "Unhandled exception",
            Event   => Event,
            Context => Context);
   end Notify;

   --------------
   --  Create  --
   --------------

   overriding
   procedure Notify (Observer : access Create_Observer;
                     Packet   : in     ESL.Packet.Instance;
                     Client   : in     ESL.Client.Reference) is
      pragma Unreferenced (Observer);
      pragma Unreferenced (Client);
      Context   : constant String      :=
        Package_Name & ".Notify (Create_Observer)";
      pragma Unreferenced (Context);
   begin
      Create_Call (From => Packet);
   end Notify;

   --------------
   --  Custom  --
   --------------

   overriding
   procedure Notify (Observer : access Custom_Observer;
                     Packet   : in     ESL.Packet.Instance;
                     Client   : in     ESL.Client.Reference) is
      use ESL;

      pragma Unreferenced (Observer);
      pragma Unreferenced (Client);
      package Constants renames PBX.Magic_Constants;

      Context   : constant String      :=
        Package_Name & ".Notify (Custom_Observer)";

      Subevent : String renames Packet.Field (Event_Subclass).Decoded_Value;

      function Is_Parking_Lot (FIFO_Name : in String) return Boolean;

      function Is_Parking_Lot (FIFO_Name : in String) return Boolean is
      begin
         if Constants.Park_Prefix'Length > FIFO_Name'Length then
            return False;
         else
            return Constants.Park_Prefix =
              FIFO_Name (Constants.Park_Prefix'Range);
         end if;

      end Is_Parking_Lot;

   begin
      --  Check which application caused the event.
      if Subevent = Constants.FIFO_Info then
         declare
            ID  : Identification renames Packet.UUID;
            Action    : String renames
              Packet.Field (FIFO_Action).Decoded_Value;
            Name : String renames
              Packet.Field (FIFO_Name).Decoded_Value;
         begin
            if Action = Constants.FIFO_Push then
               if not Is_Parking_Lot (FIFO_Name => Name) then
                  Call.Get (Call => ID).Change_State (New_State => Queued);
               else
                  Call.Get (Call => ID).Change_State (New_State => Parked);
               end if;

            elsif
              Action = Constants.FIFO_Pop   or
              Action = Constants.FIFO_Abort
            then
               Call.Get (Call => ID).Change_State (New_State => Transferring);

               if not Is_Parking_Lot (FIFO_Name => Name) then
                  Call.Get (Call => ID).Change_State (New_State => Left_Queue);
               else
                  Call.Get (Call => ID).Change_State (New_State => Unparked);
               end if;
            end if;
         end;
      else
         System_Messages.Information
           (Message => "Unhandled subevent: " & Subevent,
            Context => Context);
      end if;
   end Notify;

   ---------------
   --  Destroy  --
   ---------------

   procedure Notify (Observer : access Destroy_Observer;
                     Packet   : in     ESL.Packet.Instance;
                     Client   : in     ESL.Client.Reference) is
      pragma Unreferenced (Observer);
      pragma Unreferenced (Client);
      Context : constant String := Package_Name & ".Notify (Destroy_Observer)";
   begin
      System_Messages.Debug
        (Message => "Hanging up " & Image (Packet.UUID),
         Context => Context);

      Get (Call => Packet.UUID).Change_State (New_State => Hungup);
   exception
      when Model.Call.Not_Found =>
         System_Messages.Error
           (Message => "Tried to hang up non-existing call " &
              Image (Packet.UUID) &
              ". Call list may be inconsistent - consider reloading.",
            Context => Context);
   end Notify;

   ---------------
   --  Execute  --
   ---------------

   procedure Notify (Observer : access Execute_Observer;
                     Packet   : in     ESL.Packet.Instance;
                     Client   : in     ESL.Client.Reference) is
      pragma Unreferenced (Observer);
      pragma Unreferenced (Client);
      Context   : constant String      :=
        Package_Name & ".Notify (Notify_Observer)";

   begin
      System_Messages.Debug (Message => "Ignoring execution of " &
                               Packet.Field (Application).Value,
                             Context => Context);
   end Notify;

   ------------
   --  Park  --
   ------------

   procedure Notify (Observer : access Park_Observer;
                     Packet   : in     ESL.Packet.Instance;
                     Client   : in     ESL.Client.Reference) is
      pragma Unreferenced (Observer);
      pragma Unreferenced (Client);
   begin
      Call.Get (Call => Packet.UUID).Change_State (New_State => Parked);
   end Notify;

   --------------------------
   --  Register_Observers  --
   --------------------------

   procedure Register_Observers is
   begin
      System_Messages.Information
        (Context => Package_Name,
         Message => "Registering observers.");
      --  AdaHeads Observer.
      Observer_List.Append
        (New_Item => new AdaHeads_Observer
           (Observing => ESL.Client.Tasking.Event_Stream
              (Client => PBX.Client,
               Stream => ESL.Packet_Keys.CUSTOM)));

      Observer_List.Append
        (new Bridge_Observer
           (Observing => ESL.Client.Tasking.Event_Stream
              (Client => PBX.Client,
               Stream => ESL.Packet_Keys.CHANNEL_BRIDGE)));

      Observer_List.Append
        (new  Channel_Hold_Observer
           (Observing => ESL.Client.Tasking.Event_Stream
              (Client => PBX.Client,
               Stream => ESL.Packet_Keys.CHANNEL_HOLD)));

      Observer_List.Append
        (New_Item => new Create_Observer
           (Observing => ESL.Client.Tasking.Event_Stream
              (Client => PBX.Client,
               Stream => ESL.Packet_Keys.CHANNEL_CREATE)));

      Observer_List.Append
        (new  Destroy_Observer
           (Observing => ESL.Client.Tasking.Event_Stream
              (Client => PBX.Client,
               Stream => ESL.Packet_Keys.CHANNEL_DESTROY)));

      Observer_List.Append
        (new  Park_Observer
           (Observing => ESL.Client.Tasking.Event_Stream
              (Client => PBX.Client,
               Stream => ESL.Packet_Keys.CHANNEL_PARK)));

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
      Observer_List.Clear;
   end Unregister_Observers;

end Model.Call.Observers;
