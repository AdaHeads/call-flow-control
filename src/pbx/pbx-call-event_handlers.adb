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

with PBX.Trace;
--  with View.Call;
--  with PBX.Action;
with PBX.Call;
with PBX.Magic_Constants;
with Model.Peer;
with ESL.Packet_Keys;
with ESL.Client.Tasking;
with Handlers.Notifications;
with Client_Notification.Queue;
with Client_Notification.Call;
with System_Messages;

package body PBX.Call.Event_Handlers is
   use PBX;
   use ESL.Packet_Keys;

   use PBX.Trace;
   --   use Model.Agent;

   Brigde : Bridge_Observer
     (Observing => ESL.Client.Tasking.Event_Stream
        (Client => PBX.Client,
         Stream => ESL.Packet_Keys.CHANNEL_BRIDGE));
   pragma Unreferenced (Brigde);

   Channel_Hold : Channel_Hold_Observer
     (Observing => ESL.Client.Tasking.Event_Stream
        (Client => PBX.Client,
         Stream => ESL.Packet_Keys.CHANNEL_HOLD));
   pragma Unreferenced (Channel_Hold);

   Create : Create_Observer
     (Observing => ESL.Client.Tasking.Event_Stream
        (Client => PBX.Client,
         Stream => ESL.Packet_Keys.CHANNEL_CREATE));
   pragma Unreferenced (Create);

   Destroy : Destroy_Observer
     (Observing => ESL.Client.Tasking.Event_Stream
        (Client => PBX.Client,
         Stream => ESL.Packet_Keys.CHANNEL_DESTROY));
   pragma Unreferenced (Destroy);

   Execute : Execute_Observer
     (Observing => ESL.Client.Tasking.Event_Stream
        (Client => PBX.Client,
         Stream => ESL.Packet_Keys.CHANNEL_EXECUTE));
   pragma Unreferenced (Execute);

   Park : Park_Observer
     (Observing => ESL.Client.Tasking.Event_Stream
        (Client => PBX.Client,
         Stream => ESL.Packet_Keys.CHANNEL_PARK));
   pragma Unreferenced (Park);

   package Notification renames Handlers.Notifications;

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

      PBX.Trace.Information
        (Context => Context,
         Message => "ID 1: " & To_String (ID_A) &
           ": ID 2: " & To_String (ID_B));
      --  Either side should be represented.
      --  TODO
      Call.Link (ID_1 => ID_A,
                 ID_2 => ID_B);

      PBX.Trace.Information
        (Message => Client_Notification.Call.Pickup
           (Get (ID_A)).To_JSON.Write,
         Context => Context);

      Notification.Broadcast
        (Client_Notification.Call.Pickup (Get (ID_A)).To_JSON);
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
      PBX.Trace.Information
        (Message => Client_Notification.Call.Park
           (C => Get (ID)).To_JSON.Write,
         Context => Context);

      Notification.Broadcast
        (Client_Notification.Call.Park
           (C => Get (ID)).To_JSON);

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
         State           => Created,
         Extension       => Extension,
         From_Extension  => From_Extension);
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
            ID  : Identification renames
              Value (Packet.Field (Unique_ID).Value);
            Action    : String renames
              Packet.Field (FIFO_Action).Decoded_Value;
            Name : String renames
              Packet.Field (FIFO_Name).Decoded_Value;
         begin
            if Action = Constants.FIFO_Push then
               if not Is_Parking_Lot (FIFO_Name => Name) then
                  Call.Get (Call => ID).Change_State (New_State => Queued);
                  Notification.Broadcast
                    (Client_Notification.Queue.Join
                       (Get (Call => ID)).To_JSON);
                  PBX.Trace.Information (Client_Notification.Queue.Join
                                         (Get (Call => ID)).To_JSON.Write);
               else
                  Notification.Broadcast
                    (Client_Notification.Call.Park
                       (Call.Get (Call => ID)).To_JSON);

                  PBX.Trace.Information (Message => "Parked call. " & ID.Image,
                                         Context => Context);
               end if;

            elsif
              Action = Constants.FIFO_Pop   or
              Action = Constants.FIFO_Abort
            then
               Call.Get (Call => ID).Change_State (New_State => Speaking);

               if not Is_Parking_Lot (FIFO_Name => Name) then
                  Notification.Broadcast
                    (Client_Notification.Queue.Leave
                       (Get (Call => ID)).To_JSON);
               else
                  Notification.Broadcast
                    (Client_Notification.Call.Unpark
                       (Call.Get (Call => ID)).To_JSON);

                  PBX.Trace.Information (Message => "Unparked call. "
                                         & ID.Image,
                                         Context => Context);
               end if;
            end if;
         end;
      else
         PBX.Trace.Information
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
      Context   : constant String      :=
        Package_Name & ".Notify (Destroy_Observer)";

      ID  : Identification renames
        Value (Packet.Field (Unique_ID).Value);
      Target_Call : Instance;
   begin

      Target_Call := Call.Remove (ID => ID);

      PBX.Trace.Information
        (Message => Client_Notification.Call.Hangup
           (Target_Call).To_JSON.Write,
         Context => Context);

      Notification.Broadcast
        (Client_Notification.Call.Hangup (Target_Call).To_JSON);
   exception
      when PBX.Call.Not_Found =>
         PBX.Trace.Error
           (Message => "Tried to hang up non-existing call " & Image (ID) &
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
      PBX.Trace.Information (Message => "Ignoring execution of " &
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
      Context   : constant String      :=
        Package_Name & ".Notify (Notify_Observer)";

      ID  : Identification renames
        Value (Packet.Field (Unique_ID).Value);

   begin

      Call.Get (Call => ID).Change_State (New_State => Parked);

      Notification.Broadcast
        (Client_Notification.Call.Park (Call.Get (Call => ID)).To_JSON);

      PBX.Trace.Information (Message => "Parked call. " & ID.Image,
                             Context => Context);
   end Notify;

end PBX.Call.Event_Handlers;
