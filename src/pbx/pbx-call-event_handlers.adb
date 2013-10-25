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
with ESL.Peer;
with ESL.Peer_ID;
with PBX.Magic_Constants;
--  with Model.Agent;
with ESL.Packet_Keys;
with ESL.Client.Tasking;
with Model.Agent_ID;
with Handlers.Notifications;
with Client_Notification.Queue;
with Client_Notification.Call;
with Ada.Strings.Unbounded;

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

   Custom : Custom_Observer
     (Observing => ESL.Client.Tasking.Event_Stream
        (Client => PBX.Client,
         Stream => ESL.Packet_Keys.CUSTOM));
   pragma Unreferenced (Custom);

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

--     procedure Dial (Packet : in ESL.Packet.Instance);
--
--     procedure Join (Packet : in ESL.Packet.Instance);
--
--     procedure Leave (Packet : in ESL.Packet.Instance);
--     --  A Leave event occurs when a channel leaves a Queue for any reason.
--     --  E.g. hangup or pickup. This procedure is responsible for
--  changing the
--     --  state of the to transferring and broadcasts the event.
--
--     procedure Hangup (Packet : in ESL.Packet.Instance);
--
--     procedure Originate_Response (Packet : in ESL.Packet.Instance);
--
--     procedure Parked_Call (Packet : in ESL.Packet.Instance);
--
--     procedure Unlink (Packet : in ESL.Packet.Instance);

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

      Get (ID_A).Assign (To => Model.Agent_ID.Create (1));

      PBX.Trace.Information
        (Context => Context,
         Message => "ID 1: " & To_String (ID_A) &
           ": ID 2: " & To_String (ID_B));
--        PBX.Trace.Information
--            (Context => Context,
--             Message => Client_Notification.Call.Pickup
--               (C => Get (ID_A)).To_JSON.Write);

      --  Either side should be represented.
      --  TODO
      Call.Link (ID_1 => ID_A,
                 ID_2 => ID_B);

--        if Call.Has (Channel1) then
--           Get (Channel1).Link (Channel => Channel2);
--           Get (Channel1).Change_State (New_State => Speaking);
--        else
--           Get (Channel2).Change_State (New_State => Speaking);
--           Get (Channel2).Link (Channel => Channel1);
--        end if;

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
   begin

      Create_And_Insert (Inbound         => True,
                         ID              => ID,
                         State           => Created,
                         Organization_ID => 1);
   end Notify;

   --------------
   --  Custom  --
   --------------

   overriding
   procedure Notify (Observer : access Custom_Observer;
                     Packet   : in     ESL.Packet.Instance;
                     Client   : in     ESL.Client.Reference) is
      use ESL;
      use Ada.Strings.Unbounded;

      pragma Unreferenced (Observer);
      pragma Unreferenced (Client);
      package Constants renames PBX.Magic_Constants;

      Context   : constant String      :=
        Package_Name & ".Notify (Custom_Observer)";

      Subevent : String renames Packet.Field (Event_Subclass).Decoded_Value;
   begin
      --  Check which application caused the event.
      if Subevent = Constants.FIFO_Info then
         declare
            ID  : Identification renames
              Value (Packet.Field (Unique_ID).Value);
            Action : String renames Packet.Field (FIFO_Action).Decoded_Value;
         begin
            if Action = Constants.FIFO_Push then
               Call.Get (Call => ID).Change_State (New_State => Queued);
               Notification.Broadcast
                 (Client_Notification.Queue.Join
                    (Get (Call => ID)).To_JSON);
               PBX.Trace.Information (Client_Notification.Queue.Join
                                      (Get (Call => ID)).To_JSON.Write);

            elsif
              Action = Constants.FIFO_Pop or
              Action = Constants.FIFO_Abort
            then
               Call.Get (Call => ID).Change_State (New_State => Speaking);
               Notification.Broadcast
                 (Client_Notification.Queue.Leave
                    (Get (Call => ID)).To_JSON);

            elsif Action = Constants.Sofia_Register then
               declare
                  ID : constant Peer_ID.Peer_ID_Type :=
                    Peer_ID.Create
                      (Channel_Kind => "SIP", --  Sofia = SIP.
                       Peername     =>
                         Packet.Field (Key => To_User).Decoded_Value);
               begin
                  if not Peer.List.Contains (Peer_ID => ID) then
                     Peer.List.Put
                       (Peer =>
                          (ID         => ID,
                           Agent_ID   => Null_Agent_ID,
                           Last_Seen  => (Never => False,
                                          Time   => Current_Time),
                           State      => Peer.Idle,
                           Last_State => Peer.Unregistered,
                           Port       => Natural'Value
                             (Packet.Field (Network_IP).Decoded_Value),
                           Address    =>  To_Unbounded_String (Packet.Field
                             (Network_Port).Decoded_Value)));
                  else
                     Peer.List.Get (Peer_ID => ID).Seen;
                  end if;
               end;
            elsif Action = Constants.Sofia_Unregister then
               declare
                  ID : constant Peer_ID.Peer_ID_Type :=
                    Peer_ID.Create
                      (Channel_Kind => "SIP", --  Sofia = SIP.
                       Peername     =>
                         Packet.Field (Key => To_User).Decoded_Value);
               begin
                  Peer.List.Get (Peer_ID => ID).Change_State
                    (Peer.Unregistered);
               end;
            else
               PBX.Trace.Information
              (Message => "Unhandled FIFO Action: " & Action,
               Context => Context);
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

   ------------
   --  Dial  --
   ------------

--     procedure Dial (Packet : in Parser.Packet_Type) is
--        Context   : constant String := Package_Name & ".Dial";
--
--        Sub_Event   : String renames Packet.Get_Value (Parser.SubEvent);
--        Channel     : Channel_Identification renames
--                        Value (Packet.Get_Value (Parser.Channel));
--     begin
--        --  There is a sequence to a Dial event represented by a SubEvent.
--        --  It consists of "Begin" or "End"
--        if Channel_ID.Value
--  (Packet.Get_Value (Parser.Channel)).Temporary then
--           AMI.Trace.Log (Debug, Context & "Ignoring temporary call with " &
--                            "channel " & Packet.Get_Value (Parser.Channel));
--           return;
--        end if;
--
--        if Sub_Event = "Begin" then
--           if not PBX.Call.Has (Channel_ID => Channel) then
--              AMI.Trace.Log (Debug, Context & "creating call:");
--              Create_And_Insert
--                (Inbound        => True,
--                 Channel        => Channel,
--                 State          => Dialing,
--                 B_Leg          =>
--                   Value (Packet.Get_Value (Parser.Destination)));
--           else
--              AMI.Trace.Log (Debug, Context & "found call:");
--              --  The call should already exist, just update the B_Leg and
--              --  set state to dialing
--
--              Get (Channel => Channel).Dial
--                (Value (Packet.Get_Value (Parser.Destination)));
--
--           end if;
--
--           if not Get (Channel => Channel).Inbound then
--              Get (Channel => Channel).Assign
--                (Model.Agent_ID.Create ("1"));
--           end if;
--
--           AMI.Trace.Log (Debug, Context & "Begin: " &  View.Call.To_JSON
--                          (Get (Channel => Channel)).Write);
--
--
--  --  When a Dial event ends, the call is over, and must thus be removed
--           --  From the call list.
--        elsif Sub_Event = "End" then
--           Get (Channel => Channel).Change_State (New_State => Ended);
--
--           AMI.Trace.Log (Debug, Context & "End: " & View.Call.To_JSON
--                          (Get (Channel => Channel)).Write);
--        else
--           AMI.Trace.Log
--             (Error, Context & ": " &
--                "unknown SubEvent: " & Sub_Event);
--        end if;
--     end Dial;
--
--     --------------
--     --  Hangup  --
--     --------------
--
--     procedure Hangup (Packet : in Parser.Packet_Type) is
--        Context : constant String := Package_Name & ".Hangup";
--        Channel : Channel_Identification renames
--                    Value (Packet.Get_Value (Parser.Channel));
--     begin
--        if Channel_ID.Value
--  (Packet.Get_Value (Parser.Channel)).Temporary then
--           AMI.Trace.Log (Debug, Context & "Ignoring temporary call with " &
--                            "channel " & Packet.Get_Value (Parser.Channel));
--           return;
--        end if;
--
--        if PBX.Call.Has (Channel_ID => Channel) then
--           AMI.Trace.Debug (Context, "Hanging up call");
--           Notification.Broadcast
--             (Client_Notification.Call.Hangup
--                (Remove (Channel_ID => Channel)).To_JSON);
--        else
--           AMI.Trace.Log (Debug, Context & " Channel not found: " &
--                            To_String (Channel) & " - ignoring the hangup.");
--        end if;
--
--     exception
--        when Call.Not_Found =>
--           AMI.Trace.Log (Error, Context & " Call with channel: " &
--                            To_String (Channel) & " not found.");
--        when others =>
--           AMI.Trace.Log (Error, Context & ": unkown exception");
--           raise;
--
--     end Hangup;

   ------------
   --  Join  --
   ------------

--     procedure Join (Packet : in Parser.Packet_Type) is
--        --  Context   : constant String := Package_Name & ".Join";
--        Channel     : Channel_Identification renames
--                        Value (Packet.Get_Value (Parser.Channel));
--        Org_ID      : Natural renames
--                        Natural'Value (Packet.Get_Value (Parser.Queue));
--     begin
--        Create_And_Insert
--          (Inbound         => True,
--           Channel         => Channel,
--           State           => Queued,
--           Organization_ID => Org_ID);
--
--        Get (Channel => Channel).Change_State (New_State => Queued);
--
--        Notification.Broadcast
--          (Client_Notification.Queue.Join
--             (Get (Channel => Channel)).To_JSON);
--     end Join;
--
--     -------------
--     --  Leave  --
--     -------------
--
--     procedure Leave (Packet : in Parser.Packet_Type) is
--        Context : constant String := Package_Name & ".Leave";
--        Channel     : Channel_Identification renames
--                        Value (Packet.Get_Value (Parser.Channel));
--     begin
--        if Channel_ID.Value
--  (Packet.Get_Value (Parser.Channel)).Temporary then
--           AMI.Trace.Log (Debug, Context & "Ignoring temporary call with " &
--                            "channel " & Packet.Get_Value (Parser.Channel));
--           return;
--        end if;
--
--        Get (Channel => Channel).Change_State (New_State => Transferring);
--
--        Notification.Broadcast
--          (Client_Notification.Queue.Leave
--             (Get (Channel => Channel)).To_JSON);
--     end Leave;

   --------------------------
   --  Originate_Response  --
   --------------------------

--     procedure Originate_Response (Packet : in Parser.Packet_Type) is
--        Context : constant String := Package_Name & ".Originate_Response";
--        Channel : Channel_Identification renames
--                    Value (Packet.Get_Value (Parser.Channel));
--        Call_ID : PBX.Call.Identification renames
--                    PBX.Action.Origination_Request
--                      (PBX.Action.Value (Packet.Action_ID));
--     begin
--        if Packet.Get_Value (Parser.Response) /= "Success" then
--           AMI.Trace.Log
--  (Error, Context & ": Originate failed, removing call " &
--                            To_String (Call_ID));
--
--           AMI.Trace.Debug (PBX.Call.Get (Call => Call_ID).To_JSON.Write);
--           Notification.Broadcast
--             (Client_Notification.Call.Originate_Failed
--                (C => PBX.Call.Remove (ID => Call_ID)).To_JSON);
--
--        else
--           PBX.Call.Confirm
--             (ID          => Call_ID,
--              Channel     => Channel);
--           AMI.Trace.Log (Debug, Context & "creating call:" &
--                            PBX.Call.Get (Call => Call_ID).To_JSON.Write);
--
--           Notification.Broadcast
--             (Client_Notification.Call.Originate_Success
--                (C => PBX.Call.Get (Call => Call_ID)).To_JSON);
--        end if;
--
--     end Originate_Response;

   -------------------
   --  Parked_Call  --
   -------------------

--     procedure Parked_Call (Packet : in Parser.Packet_Type) is
--        Context : constant String := Package_Name & ".Parked_Call";
--        Channel : Channel_Identification renames
--                    Value (Packet.Get_Value (Parser.Channel));
--     begin
--        if Channel_ID.Value
--  (Packet.Get_Value (Parser.Channel)).Temporary then
--           AMI.Trace.Log (Debug, Context & "Ignoring temporary call with " &
--                            "channel " & Packet.Get_Value (Parser.Channel));
--           return;
--        end if;
--
--        Get (Channel).Change_State (New_State => Parked);
--        --  Broadcast it.
--        Notification.Broadcast
--          (Client_Notification.Call.Park (C => Get (Channel)).To_JSON);
--        AMI.Trace.Log (Debug, Client_Notification.Call.Park
--                                (Get (Channel)).To_JSON.Write);
--     end Parked_Call;

   ------------------------
   --  Register_Handers  --
   ------------------------

--     procedure Register_Handlers is
--     begin
--     end Register_Handlers;

   --------------
   --  Unlink  --
   --------------

--     procedure Unlink (Packet : in Parser.Packet_Type) is
--        Context : constant String := Package_Name & ".Parked_Call";
--        Channel     : Channel_Identification renames
--                        Value (Packet.Get_Value (Parser.Channel1));
--     begin
--        --  Ignore temporary channels
--        if Channel_ID.Value (To_String (Channel)).Temporary then
--           AMI.Trace.Log (Debug, Context & "Ignoring temporary call with " &
--                            "channel " & To_String (Channel));
--           return;
--        end if;
--
--        Call.Get (Channel).Unlink;
--
--     end Unlink;

--  begin
--     Register_Handlers;
end PBX.Call.Event_Handlers;
