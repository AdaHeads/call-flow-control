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

with PBX.Magic_Constants;
with PBX.Event_Stream;
with ESL.Packet_Keys;
with System_Messages;
with Model.Origination_Requests,
     Model.Transfer_Requests;

package body Model.Call.Observers is
   use ESL.Packet_Keys;

   function "=" (Left, Right : in AdaHeads_Observer) return Boolean is
   begin
      return
        Left.ID              = Right.ID and
        Left.Observing_Event = Right.Observing_Event;
   end "=";

   function "=" (Left, Right : in Bridge_Observer) return Boolean is
   begin
      return
        Left.ID              = Right.ID and
        Left.Observing_Event = Right.Observing_Event;
   end "=";

   function "=" (Left, Right : in Channel_Hold_Observer) return Boolean is
   begin
      return
        Left.ID              = Right.ID and
        Left.Observing_Event = Right.Observing_Event;
   end "=";

   function "=" (Left, Right : in Create_Observer) return Boolean is
   begin
      return
        Left.ID              = Right.ID and
        Left.Observing_Event = Right.Observing_Event;
   end "=";

   function "=" (Left, Right : in Destroy_Observer) return Boolean is
   begin
      return
        Left.ID              = Right.ID and
        Left.Observing_Event = Right.Observing_Event;
   end "=";

   function "=" (Left, Right : in Park_Observer) return Boolean is
   begin
      return
        Left.ID              = Right.ID and
        Left.Observing_Event = Right.Observing_Event;
   end "=";

   function "=" (Left, Right : in Channel_State_Observer) return Boolean is
   begin
      return
        Left.ID              = Right.ID and
        Left.Observing_Event = Right.Observing_Event;
   end "=";

   package Constants renames PBX.Magic_Constants;

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

   procedure Notify (Observer : in AdaHeads_Observer;
                     Packet : in ESL.Packet.Instance) is
      pragma Unreferenced (Observer);

      Context : constant String :=
        Package_Name & ".Notify (AdaHeads Subclass Observer)";
   begin
      if Packet.Subevent = Constants.Prequeue_Enter then
         Get (Packet.UUID).Mark_As_Call;

         Get (Packet.UUID).Set_Reception_ID
           (Reception_Identifier'Value
              (Packet.Variables.Get
                 (Key     => Constants.Reception_ID,
                  Default => Null_Reception_Identifier'Img)));
         Get (Packet.UUID).Change_State (New_State => Created);

      elsif Packet.Subevent = Constants.Outbound_Call then
         System_Messages.Debug (Message => "Outbound call:" &
                                  Packet.UUID.Image,
                                Context => Context);

         Model.Origination_Requests.Create (ID => Packet.UUID);

         declare
            R_ID : Reception_Identifier renames
              Reception_Identifier'Value
                (Packet.Variables.Get
                     (Key     => Constants.Reception_ID,
                      Default => Null_Reception_Identifier'Img));
            C_ID : Contact_Identifier renames
              Contact_Identifier'Value
                (Packet.Variables.Get
                     (Key     => Constants.Contact_ID,
                      Default => Null_Contact_Identifier'Img));
            U_ID : Model.User_Identifier renames
              Model.User_Identifier'Value
                (Packet.Variables.Get
                     (Key     => Constants.Owner,
                      Default => Null_Contact_Identifier'Img));
         begin
            if R_ID /= Null_Reception_Identifier then
               Get (Packet.UUID).Set_Outbound_Parameters
                 (R_ID => R_ID,
                  C_ID => C_ID,
                  U_ID => U_ID);
            end if;
         end;

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
      procedure Notify (Observer : in Bridge_Observer;
                     Packet   : in ESL.Packet.Instance) is
      pragma Unreferenced (Observer);

      Context   : constant String      :=
        Package_Name & ".Notify (Bridge_Observer)";
      ID_A  : Identification renames
        Value (Packet.Field (Bridge_A_Unique_ID).Value);
      ID_B  : Identification renames
        Value (Packet.Field (Bridge_B_Unique_ID).Value);
   begin
      System_Messages.Debug
        (Message => "Bridging " & ID_A.Image & " and " & ID_B.Image,
         Context => Context);

      --  Inherit the context from the other channel.
      if Get (ID_A).Reception_ID = Null_Reception_Identifier then
         Get (ID_A).Set_Outbound_Parameters
           (R_ID => Get (ID_B).Reception_ID,
            C_ID => Get (ID_B).Contact_ID,
            U_ID => Get (ID_B).Assigned_To);
      elsif Get (ID_B).Reception_ID = Null_Reception_Identifier then
         Get (ID_B).Set_Outbound_Parameters
           (R_ID => Get (ID_A).Reception_ID,
            C_ID => Get (ID_A).Contact_ID,
            U_ID => Get (ID_A).Assigned_To);
      end if;

      Call.Link (ID_1 => ID_A,
                 ID_2 => ID_B);

      if Model.Transfer_Requests.Is_Transfer_Request ((ID_A, ID_B)) then
         Model.Transfer_Requests.Confirm ((ID_A, ID_B));
         Get (ID_A).Change_State (New_State => Transferred);
         Get (ID_B).Change_State (New_State => Transferred);
      else
         if
           Model.Origination_Requests.Is_Origination_Request (ID => ID_A)
         then
            Model.Origination_Requests.Confirm (ID => ID_A);
         elsif
           Model.Origination_Requests.Is_Origination_Request (ID => ID_B)
         then
            Model.Origination_Requests.Confirm (ID => ID_B);
         end if;

         Get (ID_A).Change_State (New_State => Speaking);
         Get (ID_B).Change_State (New_State => Speaking);
      end if;

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
      procedure Notify (Observer : in Channel_Hold_Observer;
                     Packet   : in ESL.Packet.Instance) is
      pragma Unreferenced (Observer);

      Context   : constant String      :=
        Package_Name & ".Notify (Channel_Hold_Observer)";

      ID  : Identification renames
        Value (Packet.Field (Unique_ID).Value);
   begin

      System_Messages.Debug
        (Message => "Holding channel " & ID.Image,
         Context => Context);
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
   procedure Notify (Observer : in Create_Observer;
                     Packet   : in ESL.Packet.Instance) is
      pragma Unreferenced (Observer);

      Context   : constant String      :=
        Package_Name & ".Notify (Create_Observer)";
   begin
      System_Messages.Debug
        (Message => "Creating new channel" & Packet.UUID.Image,
         Context => Context);
      Create_Call (From => Packet);
   end Notify;

   ---------------
   --  Destroy  --
   ---------------

   procedure Notify (Observer : in Destroy_Observer;
                     Packet   : in ESL.Packet.Instance) is
      pragma Unreferenced (Observer);

      Context : constant String := Package_Name & ".Notify (Destroy_Observer)";
   begin
      System_Messages.Debug
        (Message => "Hanging up " & Image (Packet.UUID),
         Context => Context);

      Get (Call => Packet.UUID).Release;  --  Remove the call assignment from
                                          --  user->call and call->user.
      Get (Call => Packet.UUID).Change_State (New_State => Hungup);
   exception
      when Model.Call.Not_Found =>
         System_Messages.Error
           (Message => "Tried to hang up non-existing call " &
              Image (Packet.UUID) &
              ". Call list may be inconsistent - consider reloading.",
            Context => Context);
   end Notify;

   ------------
   --  Park  --
   ------------

   procedure Notify (Observer : in Park_Observer;
                     Packet   : in ESL.Packet.Instance) is
      pragma Unreferenced (Observer);

      Context : constant String := Package_Name & ".Notify (Park_Observer)";
   begin

      System_Messages.Debug
        (Message => "Parking channel " & Packet.UUID.Image,
         Context => Context);

      Call.Get (Call => Packet.UUID).Mark_As_Call;
      Call.Get (Call => Packet.UUID).Change_State (New_State => Parked);
   end Notify;

   ---------------------
   --  Channel_State  --
   ---------------------

   procedure Notify (Observer : in Channel_State_Observer;
                     Packet   : in ESL.Packet.Instance) is
      pragma Unreferenced (Observer);

      Context : constant String := Package_Name &
        ".Notify (Channel_State_Observer)";
   begin

      if
        Packet.Field (Channel_Call_State).Decoded_Value = "RINGING" and
      then
        Model.Origination_Requests.Is_Origination_Request (Packet.Other_Leg)
      then
         if Get (Packet.UUID).Reception_ID = Null_Reception_Identifier then
            Get (Packet.UUID).Set_Outbound_Parameters
              (R_ID => Get (Packet.Other_Leg).Reception_ID,
               C_ID => Get (Packet.Other_Leg).Contact_ID,
               U_ID => Get (Packet.Other_Leg).Assigned_To);
         end if;

         Call.Get (Call => Packet.UUID).Change_State (New_State => Ringing);

         System_Messages.Debug
           (Packet.UUID.Image & " is origination_request and other_leg is "
            & Packet.Other_Leg.Image, Context);

      end if;

   end Notify;

   --------------------------
   --  Register_Observers  --
   --------------------------

   procedure Register_Observers is
      Context : constant String := Package_Name & ".Register_Observers";
   begin
      System_Messages.Information
        (Context => Context,
         Message => "Registering observers.");

      PBX.Event_Stream.Observer_Map.Register_Observer
        (Observer => AdaHeads_Observer'
           (Observing_Event => ESL.Packet_Keys.CUSTOM,
            ID              => <>));

      PBX.Event_Stream.Observer_Map.Register_Observer
        (Observer => Bridge_Observer'
           (Observing_Event => ESL.Packet_Keys.CHANNEL_BRIDGE,
            ID              => <>));

      PBX.Event_Stream.Observer_Map.Register_Observer
        (Observer => Channel_Hold_Observer'
           (Observing_Event => ESL.Packet_Keys.CHANNEL_HOLD,
            ID              => <>));

      PBX.Event_Stream.Observer_Map.Register_Observer
        (Observer => Create_Observer'
           (Observing_Event => ESL.Packet_Keys.CHANNEL_CREATE,
            ID              => <>));

      PBX.Event_Stream.Observer_Map.Register_Observer
        (Observer => Destroy_Observer'
           (Observing_Event => ESL.Packet_Keys.CHANNEL_DESTROY,
            ID              => <>));

      PBX.Event_Stream.Observer_Map.Register_Observer
        (Observer => Park_Observer'
           (Observing_Event => ESL.Packet_Keys.CHANNEL_PARK,
            ID              => <>));

      PBX.Event_Stream.Observer_Map.Register_Observer
        (Observer => Channel_State_Observer'
           (Observing_Event => ESL.Packet_Keys.CHANNEL_CALLSTATE,
            ID              => <>));
   end Register_Observers;

   ----------------------------
   --  Unregister_Observers  --
   ----------------------------

   procedure Unregister_Observers is
   begin
      System_Messages.Fixme
        (Context => Package_Name,
         Message => "Unregistering observers.");
      raise Program_Error with "Not implemented!";
   end Unregister_Observers;

end Model.Call.Observers;
