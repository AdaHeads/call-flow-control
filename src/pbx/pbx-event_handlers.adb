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

with Ada.Strings.Unbounded;
with Common;
with System_Messages;

with AMI.Event;
with AMI.Observers;
with Model.Agent;
with Model.Call;
with Model.Call_ID;
with Model.Calls;
with AMI.Channel;
with AMI.Channel_ID;
with AMI.Peer;
with AMI.Peer_ID;
with Handlers.Notifications;
with Client_Notification.Queue;
with Client_Notification.Call;

package body PBX.Event_Handlers is
   use Common;
   use System_Messages;
   use Ada.Strings.Unbounded;
   use PBX;
   use Model;
   use Model.Call;
   use Model.Call_ID;
   use AMI.Peer;

   package Notifications renames Handlers.Notifications;

   --------------
   --  Bridge  --
   --------------

   procedure Bridge (Packet : in Parser.Packet_Type) is
      ID1, ID2 : Call_ID.Call_ID_Type := Null_Call_ID;
   begin
      ID1 := Call_ID.Create
        (To_String (Packet.Fields.Element (Parser.UniqueID1)));
      ID2 := Call_ID.Create
        (To_String (Packet.Fields.Element (Parser.UniqueID2)));
      Calls.List.Link (ID1 => ID1,
                       ID2 => ID2);
   end Bridge;

   -------------------------
   --  Core_Show_Channel  --
   -------------------------

   --  TODO: Move this to a consistency_check package, and use more
   --  of the information.
   procedure Core_Show_Channel (Packet : in Parser.Packet_Type) is
      Requested_Channel : AMI.Channel.Instance := AMI.Channel.Null_Object;
   begin
      --  Only load missing channels.
      if not Channel.List.Contains (Key => Requested_Channel.ID) then
         Requested_Channel := AMI.Channel.Create (Packet);
         Channel.List.Insert (Requested_Channel);
      end if;
   end Core_Show_Channel;

   -----------------------------------
   --  Core_Show_Channels_Complete  --
   -----------------------------------

   --  Occurs at the end of a set of CoreShowChannel events.
   procedure Core_Show_Channels_Complete (Packet : in Parser.Packet_Type) is
      Context          : constant String :=
                           "Core_Show_Channels_Complete";
      Number_Of_Events : constant Natural :=
        Natural'Value (To_String (Packet.Fields.Element (Parser.ListItems)));
   begin
      System_Messages.Notify (Debug, "Core_Show_Channel_Complete");
      if Number_Of_Events /= Channel.List.Length then
         System_Messages.Notify (Error, Package_Name & "." & Context & ": " &
                                "Channel list inconsistent!");
      end if;
   end Core_Show_Channels_Complete;

   procedure Default_Callback (Packet : in Parser.Packet_Type) is
      --  Context : constant String := "Default_Callback";
   begin
      null;
      --  System_Messages.Notify (Debug, Package_Name & "." & Context & ": " &
      --                          To_String (Packet.Header.Value));
   end Default_Callback;

   ------------
   --  Dial  --
   ------------

   procedure Dial (Packet : in Parser.Packet_Type) is
      Context : constant String := "Dial";
      Call    : Call_Type       := Null_Call;
   begin
      --  There is a sequence to a Dial event represented by a SubEvent.
      --  It consists of "Begin" or "End"
      if To_String (Packet.Fields.Element (Parser.SubEvent)) = "Begin" then
         System_Messages.Notify (Debug, Package_Name & "." & Context & ": " &
                                   "Dial Begin");
         Call.ID := Model.Call_ID.Create
           (To_String (Packet.Fields.Element (Parser.DestUniqueID)));
         Call.Channel := Channel_ID.Value
           (To_String (Packet.Fields.Element (Parser.Destination)));
         Call.Inbound := False; --  A dial event implies outbound.
         Call.Arrived := Current_Time;

         Model.Calls.List.Insert (Call => Call);

      --  When a Dial event ends, the call is over, and must thus be removed
      --  From the call list.
      elsif To_String (Packet.Fields.Element (Parser.SubEvent)) = "End" then
         System_Messages.Notify (Debug, Package_Name & "." & Context & ": " &
                                   "Dial End");
         Call.ID := Model.Call_ID.Create (Packet.Get_Value (Parser.UniqueID));
      else
         System_Messages.Notify
           (Error, Package_Name & "." & Context & ": " &
              "unknown SubEvent: " &
              To_String (Packet.Fields.Element (Parser.SubEvent)));
      end if;
   end Dial;

   --------------
   --  Hangup  --
   --------------

   procedure Hangup (Packet : in Parser.Packet_Type)
   is
      Context        : constant String      := Package_Name & ".Hangup";
      Hungup_Channel : Channel_ID.Instance  := Channel_ID.Null_Channel_ID;
      Call           : Call_ID.Call_ID_Type := Call_ID.Null_Call_ID;
   begin
      Hungup_Channel
        := Channel_ID.Value (Packet.Get_Value (Parser.Channel));
      Call    := Call_ID.Create (Packet.Get_Value (Parser.UniqueID));

      Channel.List.Remove (Hungup_Channel);

      Model.Calls.List.Remove (Call);

      System_Messages.Notify
        (Debug, Package_Name & ".Hangup: Removed channel " &
         Hungup_Channel.Image & " and call " & Call.To_String);

   exception
      when Calls.Call_Not_Found =>
         System_Messages.Notify
           (Error, Context & ": Call not found" & Call.To_String);
      when Channel.Not_Found =>
         System_Messages.Notify (Error, Context & ": " & Hungup_Channel.Image);
   end Hangup;

   ------------
   --  Join  --
   ------------

   procedure Join (Packet : in Parser.Packet_Type) is
      Call         : Call_Type := Null_Call;
      Temp_Value   : Unbounded_String;
   begin
      Call.ID := Call_ID.Create (Packet.Get_Value (Parser.UniqueID));
      Call.Inbound := True;  --  Join implies an inbound call.

      --  See if the call already exists
      if Model.Calls.List.Contains (Call_ID => Call.ID) then
         Call := Model.Calls.List.Get (Call.ID);
         Call.State := Requeued;
      else
         Call.State := Newly_Arrived;
         Call.Arrived := Current_Time;
      end if;

      Call.Queue := Packet.Fields.Element (Parser.Queue);
      Call.Channel := Channel_ID.Value
        (To_String (Packet.Fields.Element (Parser.Channel)));
      Model.Calls.List.Insert (Call);

      Notifications.Broadcast
        (Client_Notification.Queue.Join (C => Call).To_JSON);
   exception
         when others =>
         System_Messages.Notify
           (Error,
            "My_Callbacks.Join: Got a Join event, " &
              "on a channel there is not in the channel list. " &
              "Channel: " &
                 To_String (Temp_Value));
   end Join;

   --  Channel: SIP/softphone1-00000046
   --  Queue: org_id1
   --  Count: 1
   --  Uniqueid: 1354278576.70
   procedure Leave (Packet : in Parser.Packet_Type) is
      --  Context : constant String := "My_Callbacks.Leave";
      Call    : Call_Type       := Null_Call;
   begin
      Call := Model.Calls.List.Get
        (Call_ID => Create (Packet.Get_Value (Parser.UniqueID)));

      Notifications.Broadcast
        (Client_Notification.Queue.Leave (C => Call).To_JSON);
   end Leave;

   --  A Newchannel event represents any channel created within asterisk.
   --  We collect every channel into a channel list and distribute them
   --  from there to either a call list or a peer channel list.
   procedure New_Channel (Packet : in Parser.Packet_Type) is
      use type Channel.Instance;
      New_Channel : constant AMI.Channel.Instance :=
                      AMI.Channel.Create (Packet => Packet);
   begin
      --  Ignore invalid channels for now.
      if New_Channel /= Channel.Null_Object then
         Channel.List.Insert (New_Channel);
         System_Messages.Notify
           (Debug, "My_Callbacks.New_Channel: Channel_List: " &
              Channel.List.To_String);
      end if;
   end New_Channel;

   --  Event: Newstate
   --  Privilege: call,all
   --  Channel: SIP/softphone1-0000000b
   --  ChannelState: 5
   --  ChannelStateDesc: Ringing
   --  CallerIDNum: 100
   --  CallerIDName:
   --  Uniqueid: 1340097427.11
   procedure New_State (Packet : in Parser.Packet_Type) is
      Context           : constant String  := "New_State";
      Channel_To_Change : Channel.Instance := Channel.Empty_Object;
   begin
      Channel_To_Change := Channel.List.Get
                              (Channel_ID.Value
                                 (Packet.Get_Value
                                    (Parser.Channel)));

      Channel_To_Change.Change_State (Packet);
      Channel.List.Update (Channel_To_Change);
   exception
      when others =>
         System_Messages.Notify (Error, Package_Name & "." & Context & ": " &
                                   "failed to update channel " &
                                   Channel_To_Change.To_String);
   end New_State;

   ------------------
   -- Parked_Call --
   ------------------

   procedure Parked_Call (Packet : in Parser.Packet_Type) is
      Call_To_Park : Call.Call_Type := Null_Call;
   begin
      Call_To_Park := Calls.List.Get
        (Call_ID.Create (Packet.Get_Value (Parser.UniqueID)));
      Call_To_Park.State := Parked;
      Calls.List.Put (Call_To_Park);

      --  Broadcast it.
      Notifications.Broadcast
        (Client_Notification.Call.Park (C => Call_To_Park).To_JSON);
   end Parked_Call;

   ----------------
   -- Peer_Entry --
   ----------------

   procedure Peer_Entry (Packet : in Parser.Packet_Type) is
      use type Agent.State;
   --  Context : constant String := "Peer_Entry";
      Peer        : Peer_Type := Null_Peer;
   begin
      --  Fetch the peer's ID.
      Peer.ID := Peer_ID.Create
        (Channel_Kind =>
           To_String (Packet.Fields.Element (Parser.ChannelType)),
         Peername     =>
           To_String (Packet.Fields.Element (Parser.ObjectName)));

      --  Set the agent field
      --  TODO

      --  TODO: Update this to the whatever format Asterisk 1.8 supports.
      if To_String (Packet.Fields.Element (Parser.IPaddress)) /= "-none-" then
         Peer.Address := Packet.Fields.Element (Parser.IPaddress);
         Peer.Port := Packet.Fields.Element (Parser.IPport);

         Peer.State := Unknown;
      else
         Peer.State := Unregistered;
      end if;

      --  Update the peer
      AMI.Peer.List.Put (Peer => Peer);

   end Peer_Entry;

   procedure Peer_List_Complete (Packet : in Parser.Packet_Type) is
      Context          : constant String :=
                           "Peer_List_Complete";
      Number_Of_Events : constant Natural :=
        Natural'Value (To_String (Packet.Fields.Element (Parser.ListItems)));
   begin
      if Number_Of_Events /= AMI.Peer.List.Count then
         System_Messages.Notify (Error, Package_Name & "." & Context & ": " &
                                   "peer list inconsistent! Got" &
                                   Number_Of_Events'Img &
                                " - expected" & AMI.Peer.List.Count'Img);
      end if;
   end Peer_List_Complete;

   -----------------
   -- Peer_Status --
   -----------------

   procedure Peer_Status (Packet : in Parser.Packet_Type) is

      Context : constant String := "Peer_Status";
      Peer    : Peer_Type := Null_Peer;
   begin

      Peer.ID := Peer_ID.Create
        (To_String (Packet.Fields.Element (AMI.Parser.Peer)));
      --  Check if the peer is known
      if AMI.Peer.List.Contains (Peer.ID) then
         Peer := AMI.Peer.List.Get (Peer.ID);
      else
         System_Messages.Notify (Critical,
                                 Package_Name & "." & Context &
                                 ": got unknown peer " & Peer.ID.To_String);
      end if;

      --  Find the agent, and assign the peer.
      --  TODO

      --  Update fields
      Peer.Seen; --  Bump timstamp.
      if Packet.Fields.Contains (AMI.Parser.Address) then
         Peer.Address := Packet.Fields.Element (Parser.Address);
      end if;

      if Packet.Fields.Contains (AMI.Parser.Port) then
         Peer.Port := Packet.Fields.Element (Parser.Port);
      end if;

      --  Setting the State - registered or not.
      if Packet.Fields.Contains (AMI.Parser.PeerStatus) then
         --  Save the previous state.
         Peer.Last_State := Peer.State;
         if To_String (Packet.Fields.Element (Parser.PeerStatus)) =
           AMI.Peer_State_Unregistered then
            Peer.State := Unregistered;

         elsif To_String (Packet.Fields.Element (Parser.PeerStatus)) =
           AMI.Peer_State_Registered then
            Peer.State := Idle;

         else
            Peer.State := Unknown;
            System_Messages.Notify
              (Critical, "My_Callbacks.Peer_Status: " &
                 "Peer changed state into an unknown state: " &
                 To_String (Packet.Fields.Element (Parser.PeerStatus)));
         end if;

         System_Messages.Notify
           (Debug, "My_Callbacks.Peer_Status: " &
              Peer.To_String);
      else
         System_Messages.Notify
           (Error, "My_Callbacks.Peer_Status: No state information supplied");
         raise Parser.BAD_PACKET_FORMAT;
      end if;

      --  Update the peer
      AMI.Peer.List.Put (Peer => Peer);

   end Peer_Status;

   --  Event: QueueCallerAbandon
   --  Privilege: agent,all
   --  Queue: org_id2
   --  Uniqueid: 1351853779.111
   --  Position: 1
   --  OriginalPosition: 1
   --  HoldTime: 14
   procedure Queue_Abandon (Packet : in Parser.Packet_Type) is
      Call              : Call_Type := Null_Call;
      Queue             : Unbounded_String := Null_Unbounded_String;
      Position          : Integer := -1;
      Original_Position : Integer := -1;
      Hold_Time         : Integer := -1;
   begin
      Call.ID := Create
        (To_String (Packet.Fields.Element (Parser.UniqueID)));

      Position := Integer'Value
        (To_String (Packet.Fields.Element (Parser.Position)));

      Queue := Packet.Fields.Element (Parser.Queue);

      Original_Position := Integer'Value
        (To_String (Packet.Fields.Element (Parser.OriginalPosition)));

      Hold_Time := Integer'Value
        (To_String (Packet.Fields.Element (Parser.HoldTime)));

      System_Messages.Notify (Debug, "My.Callbacks.Queue_Abandon: Call_ID " &
                                To_String (Call.ID) & " left queue " &
                                To_String (Queue) & " after" & Hold_Time'Img &
                                " seconds. Position:" & Position'Img & "," &
                                " original position" & Original_Position'Img);
   end Queue_Abandon;

begin
   AMI.Observers.Register (Event   => AMI.Event.Bridge,
                           Handler => Bridge'Access);

   AMI.Observers.Register (Event   => AMI.Event.CoreShowChannel,
                           Handler => Core_Show_Channel'Access);
   AMI.Observers.Register (Event   => AMI.Event.CoreShowChannelsComplete,
                           Handler => Core_Show_Channels_Complete'Access);
   AMI.Observers.Register (Event   => AMI.Event.PeerStatus,
                           Handler => Peer_Status'Access);
   AMI.Observers.Register (Event   => AMI.Event.PeerEntry,
                           Handler => Peer_Entry'Access);
   AMI.Observers.Register (Event   => AMI.Event.PeerlistComplete,
                           Handler => Peer_List_Complete'Access);
   AMI.Observers.Register (Event   => AMI.Event.Hangup,
                           Handler => Hangup'Access);
   AMI.Observers.Register (Event   => AMI.Event.Join,
                           Handler => Join'Access);
   AMI.Observers.Register (Event   => AMI.Event.Leave,
                           Handler => Leave'Access);
   AMI.Observers.Register (Event   => AMI.Event.Newchannel,
                           Handler => New_Channel'Access);
   AMI.Observers.Register (Event   => AMI.Event.Newstate,
                           Handler => New_State'Access);
   AMI.Observers.Register (Event   => AMI.Event.Dial,
                           Handler => Dial'Access);
   AMI.Observers.Register (Event   => AMI.Event.QueueCallerAbandon,
                           Handler => Queue_Abandon'Access);
   AMI.Observers.Register (Event   => AMI.Event.ParkedCall,
                           Handler => Parked_Call'Access);
end PBX.Event_Handlers;
