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

with Model.Agents;
with Model.Call;
with Model.Calls;
with Model.Channel;
with Model.Channels;
with Model.Channel_ID;
with Model.Peers;
with Model.Peer_ID;
with Handlers.Notifications;
with Model.Call_ID;
with JSON.Event;

package body My_Callbacks is
   use Common;
   use System_Messages;
   use Ada.Strings.Unbounded;
   use Model.Call;
   use Model.Call_ID;
   use Model.Channel;
   use Model.Channel_ID;
   use Model.Peers;
   use Model.Peer_ID;

   package Notifications renames Handlers.Notifications;

   --------------
   --  Agents  --
   --------------

   procedure Agents is
   begin
      System_Messages.Notify (Debug, "Agents not implemented");
   end Agents;

   --  Lists agents
   --  Event: Bridge
   --  Privilege: call,all
   --  Bridgestate: Link
   --  Bridgetype: core
   --  Channel1: SIP/softphone2-0000000a
   --  Channel2: SIP/softphone1-0000000b
   --  Uniqueid1: 1340097427.10
   --  Uniqueid2: 1340097427.11
   --  CallerID1: softphone2
   --  CallerID2: 100
   procedure Bridge is
   begin
      System_Messages.Notify (Debug, "Bridge not implemented");
   end Bridge;

   --  Event: CoreShowChannel
   --  Channel: SIP/softphone1-0000003f
   --  UniqueID: 1354109368.63
   --  Context: LocalSets
   --  Extension: 7001
   --  Priority: 5
   --  ChannelState: 6
   --  ChannelStateDesc: Up
   --  Application: Queue
   --  ApplicationData: org_id1
   --  CallerIDnum: softphone1
   --  Duration: 54:08:10
   --  AccountCode:
   --  BridgedChannel:
   --  BridgedUniqueID:
   procedure Core_Show_Channel (Packet : in Packet_Type) is
      Channel : Model.Channel.Channel_Type := Null_Channel;
   begin
      System_Messages.Notify (Debug, "Core_Show_Channel: ");

      if not Model.Channels.List.Contains (Channel_ID => Channel.ID) then
         Channel.ID        :=
           Create (To_String (Packet.Fields.Element (AMI.Parser.Channel)));
         Channel.State     :=
           Model.Channel.To_Channel_State
             (To_String (Packet.Fields.Element (AMI.Parser.ChannelState)));
         Channel.Description :=
           Packet.Fields.Element (AMI.Parser.ChannelStateDesc);
         Channel.CallerIDNum :=
           Packet.Fields.Element (AMI.Parser.CallerIDNum);
         Channel.Call_ID := Create
           (To_String (Packet.Fields.Element (AMI.Parser.Uniqueid)));
         Model.Channels.List.Insert (Channel);
      end if;
   end Core_Show_Channel;

   --  Occurs at the end of a set of CoreShowChannel events.
   procedure Core_Show_Channels_Complete (Packet : in Packet_Type) is
      Context          : constant String :=
                           "Core_Show_Channels_Complete";
      Number_Of_Events : constant Natural :=
        Natural'Value (To_String (Packet.Fields.Element (ListItems)));
   begin
      System_Messages.Notify (Debug, "Core_Show_Channel_Complete");
      if Number_Of_Events /= Model.Channels.List.Length then
         System_Messages.Notify (Error, Package_Name & "." & Context & ": " &
                                "Channel list inconsistent!");
      end if;
   end Core_Show_Channels_Complete;

   procedure Default_Callback (Packet : in Packet_Type) is
      --  Context : constant String := "Default_Callback";
   begin
      null;
      --  System_Messages.Notify (Debug, Package_Name & "." & Context & ": " &
      --                          To_String (Packet.Header.Value));
   end Default_Callback;

   --  A dial event occurs when a peer actively dials an extension.
   procedure Dial (Packet : in Packet_Type) is
      Context : constant String := "Dial";
      Call    : Call_Type       := Null_Call;
   begin
      --  There is a sequence to a Dial event represented by a SubEvent.
      --  It consists of "Begin" or "End"
      if To_String (Packet.Fields.Element (SubEvent)) = "Begin" then
         System_Messages.Notify (Debug, Package_Name & "." & Context & ": " &
                                   "Dial Begin");
         Call.ID := Model.Call_ID.Create
           (To_String (Packet.Fields.Element (Uniqueid)));
         Call.Channel_ID := Model.Channel_ID.Create
           (To_String (Packet.Fields.Element (Channel)));
         Call.Inbound := False; --  A dial event implies outbound.
         Call.Arrived := Current_Time;

         Model.Calls.List.Insert (Call => Call);

      --  When a Dial event ends, the call is over, and must thus be removed
      --  From the call list.
      elsif To_String (Packet.Fields.Element (SubEvent)) = "End" then
         System_Messages.Notify (Debug, Package_Name & "." & Context & ": " &
                                   "Dial End");
         Call.ID := Model.Call_ID.Create
           (To_String (Packet.Fields.Element (Uniqueid)));
         Model.Calls.List.Remove (Call_ID => Call.ID);
      else
         System_Messages.Notify
           (Error, Package_Name & "." & Context & ": " &
              "unknown SubEvent: " &
              To_String (Packet.Fields.Element (SubEvent)));
      end if;
   end Dial;

   --  Clear out channels
   procedure Hangup (Packet : in Packet_Type)
   is
      Context    : constant String :=  Package_Name & ".Hangup";
      Channel_ID : constant Channel_ID_Type :=
                     Create (To_String (Packet.Fields.Element (Channel)));
   begin
      if Model.Channels.List.Contains (Channel_ID => Channel_ID) then
         Model.Channels.List.Remove (Channel_ID => Channel_ID);
         System_Messages.Notify
           (Debug, Package_Name & ".Hangup: Removed channel " &
              Channel_ID.To_String);
      else
         System_Messages.Notify
           (Error, Package_Name & ".Hangup: Channel not found" &
              Channel_ID.To_String);
      end if;
   exception
         when others =>
         System_Messages.Notify (Error, Context &
                                   ": Hangup failed on channel " &
                                   Channel_ID.To_String);
   end Hangup;

   procedure Join (Packet : in Packet_Type) is
      Call       : Call_Type := Null_Call;
      Temp_Value : Unbounded_String;
   begin
      Call.ID := Create (To_String (Packet.Fields.Element (Uniqueid)));
      Call.Inbound := True;  --  Join implies an inbound call.

      --  See if the call already exists
      if Model.Calls.List.Contains (Call_ID => Call.ID) then
         Call := Model.Calls.List.Get (Call.ID);
         Call.State := Requeued;
      else
         Call.State := Newly_Arrived;
         Call.Arrived := Current_Time;
      end if;

      Call.Queue := Packet.Fields.Element (Queue);
      Call.Channel_ID := Create (To_String (Packet.Fields.Element (Channel)));

      System_Messages.Notify
        (Debug, "My_Callbacks.Join: Inserting call: " & Call.To_String);

      Model.Calls.List.Insert (Call);

      Notifications.Broadcast (JSON.Event.New_Call_JSON_String (Call));
   exception
         when others =>
         System_Messages.Notify
           (Error,
            "My_Callbacks.Join: Got a Join event, " &
              "on a channel there is not in the channel list. " &
              "Channel: " &
                 To_String (Temp_Value));
   end Join;

   --  A Leave event occurs when a channel leaves a Queue for any reason.
   --  E.g. hangup or pickup. This is responsible cleaning up pending calls,
   --  but does not touch the channel - as is can still be acive.
   --  Channel: SIP/softphone1-00000046
   --  Queue: org_id1
   --  Count: 1
   --  Uniqueid: 1354278576.70
   procedure Leave (Packet : in Packet_Type) is
      Context : constant String       := "My_Callbacks.Leave";
      Call_ID : constant Call_ID_Type := Create
        (To_String (Packet.Fields.Element (AMI.Parser.Uniqueid)));
      Call    : constant Call_Type :=
                  Model.Calls.List.Get (Call_ID => Call_ID);
   begin
      System_Messages.Notify
        (Debug, Context & ": Removing call " & Call_ID.To_String);
      Model.Calls.List.Remove (Call_ID => Call_ID);
      Notifications.Broadcast (JSON.Event.Hangup_JSON_String (Call));
   end Leave;

   --  A Newchannel event represents any channel created within asterisk.
   --  We collect every channel into a channel list and distribute them
   --  from there to either a call list or a peer channel list.
   procedure New_Channel (Packet : in Packet_Type) is
      Channel : Model.Channel.Channel_Type := Null_Channel;
   begin
      Channel.ID        :=
        Create (To_String (Packet.Fields.Element (AMI.Parser.Channel)));
      Channel.State     :=
        Model.Channel.To_Channel_State
          (To_String (Packet.Fields.Element (AMI.Parser.ChannelState)));
      Channel.Description :=
        Packet.Fields.Element (AMI.Parser.ChannelStateDesc);
      Channel.CallerIDNum :=
        Packet.Fields.Element (AMI.Parser.CallerIDNum);
      Channel.CallerIDName :=
        Packet.Fields.Element (AMI.Parser.CallerIDName);
      Channel.Extension := Packet.Fields.Element (AMI.Parser.Exten);
      Channel.AccountCode :=
        Packet.Fields.Element (AMI.Parser.AccountCode);
      Channel.Context :=
        Packet.Fields.Element (AMI.Parser.Context);
      Channel.Call_ID := Create
        (To_String (Packet.Fields.Element (AMI.Parser.Uniqueid)));

      Model.Channels.List.Insert (Channel => Channel);
      System_Messages.Notify
        (Debug, "My_Callbacks.New_Channel: Channel_List: " &
           Model.Channels.List.To_String);
   end New_Channel;

   --  Event: Newstate
   --  Privilege: call,all
   --  Channel: SIP/softphone1-0000000b
   --  ChannelState: 5
   --  ChannelStateDesc: Ringing
   --  CallerIDNum: 100
   --  CallerIDName:
   --  Uniqueid: 1340097427.11
   procedure New_State (Packet : in Packet_Type) is
      use Model;
      Context : constant String            := "New_State";
      Channel : Model.Channel.Channel_Type := Null_Channel;
   begin
      --  Fetch the previous channel image
      Channel := Channels.List.Get
        (Create (To_String (Packet.Fields.Element (AMI.Parser.Channel))));

      --  Update the fields
      Channel.State := To_Channel_State
        (To_String (Packet.Fields.Element (ChannelState)));

      Channel.Description  := Packet.Fields.Element (ChannelStateDesc);
      Channel.CallerIDNum  := Packet.Fields.Element (CallerIDNum);
      Channel.CallerIDName := Packet.Fields.Element (CallerIDName);

      Channels.List.Update (Channel);
   exception
      when others =>
         System_Messages.Notify (Error, Package_Name & "." & Context & ": " &
                                   "failed to update channel " &
                                   Channel.To_String);

   end New_State;

   ----------------
   -- Peer_Entry --
   ----------------

   procedure Peer_Entry (Packet : in Packet_Type) is
      --  Context : constant String := "Peer_Entry";
      Peer    : Peer_Type := Null_Peer;
   begin
      --  Fetch the peer's ID.
      Peer.ID := Create
        (Channel_Kind =>
           To_String (Packet.Fields.Element (AMI.Parser.ChannelType)),
         Peername     =>
           To_String (Packet.Fields.Element (AMI.Parser.ObjectName)));

      --  Set the agent field
      Model.Agents.Get (Peer_ID => Peer.ID).Assign (Peer => Peer);

      if To_String (Packet.Fields.Element (IPaddress)) /= "-none-" then
         Peer.Address := Packet.Fields.Element (IPaddress);
         Peer.Port := Packet.Fields.Element (IPport);

         Peer.State := Unknown;
      else
         Peer.State := Unregistered;
      end if;

      --  Update the peer
      Model.Peers.List.Put (Peer => Peer);

      --  Let the clients know about the change. But only on "real" changes.
      if Peer.Last_State /= Peer.State then
         Notifications.Broadcast (JSON.Event.Agent_State_JSON_String (Peer));
      end if;

   end Peer_Entry;

   procedure Peer_List_Complete (Packet : in Packet_Type) is
      Context          : constant String :=
                           "Peer_List_Complete";
      Number_Of_Events : constant Natural :=
        Natural'Value (To_String (Packet.Fields.Element (ListItems)));
   begin
      if Number_Of_Events /= Model.Peers.List.Count then
         System_Messages.Notify (Error, Package_Name & "." & Context & ": " &
                                   "peer list inconsistent! Got" &
                                   Number_Of_Events'Img &
                                " - expected" & Model.Peers.List.Count'Img);
      end if;
   end Peer_List_Complete;

   -----------------
   -- Peer_Status --
   -----------------

   procedure Peer_Status (Packet : in Packet_Type) is

      Context : constant String := "Peer_Status";
      Peer    : Peer_Type := Null_Peer;

      Buffer  : Unbounded_String;
   begin

      Peer.ID := Create (To_String (Packet.Fields.Element (AMI.Parser.Peer)));
      --  Check if the peer is known
      if Model.Peers.List.Contains (Peer.ID) then
         Peer := Model.Peers.List.Get (Peer.ID);
      else
         System_Messages.Notify (Critical,
                                 Package_Name & "." & Context &
                                 ": got unknown peer " & Peer.ID.To_String);
      end if;

      --  Set the agent field
      Model.Agents.Get (Peer_ID => Peer.ID).Assign (Peer => Peer);

      --  Update fields
      Peer.Seen; --  Bump timstamp.
      if Packet.Fields.Contains (AMI.Parser.Address) then
         Peer.Address := Packet.Fields.Element (Address);
      end if;

      if Packet.Fields.Contains (AMI.Parser.Port) then
         Peer.Port := Packet.Fields.Element (Port);
      end if;

      --  Setting the State - registered or not.
      if Packet.Fields.Contains (AMI.Parser.PeerStatus) then
         Buffer := Packet.Fields.Element (PeerStatus);
         --  Save the previous state.
         Peer.Last_State := Peer.State;
         if To_String (Buffer) = AMI.Peer_State_Unregistered then
            Peer.State := Unregistered;

         elsif To_String (Buffer) = AMI.Peer_State_Registered then
            Peer.State := Idle;

         else
            Peer.State := Unknown;
            System_Messages.Notify
              (Critical, "My_Callbacks.Peer_Status: " &
                 "Peer changed state into an unknown state: " &
                 To_String (Buffer));
         end if;

         System_Messages.Notify
           (Debug, "My_Callbacks.Peer_Status: " &
              Peer.To_String);
      else
         System_Messages.Notify
           (Error, "My_Callbacks.Peer_Status: No state information supplied " &
              Image (Packet));
         raise BAD_PACKET_FORMAT;
      end if;

      --  Update the peer
      Model.Peers.List.Put (Peer => Peer);

      --  Let the clients know about the change. But only on "real" changes.
      if Peer.Last_State /= Peer.State then
         Notifications.Broadcast (JSON.Event.Agent_State_JSON_String (Peer));
      end if;
   end Peer_Status;

   --  Event: QueueCallerAbandon
   --  Privilege: agent,all
   --  Queue: org_id2
   --  Uniqueid: 1351853779.111
   --  Position: 1
   --  OriginalPosition: 1
   --  HoldTime: 14
   procedure Queue_Abandon (Packet : in Packet_Type) is
      Call              : Call_Type := Null_Call;
      Buffer            : Unbounded_String := Null_Unbounded_String;
      Queue             : Unbounded_String := Null_Unbounded_String;
      Position          : Integer := -1;
      Original_Position : Integer := -1;
      Hold_Time         : Integer := -1;
   begin
      if Try_Get (Packet.Fields, AMI.Parser.Uniqueid, Buffer) then
         Call.ID := Create (To_String (Buffer));
      end if;

      if Try_Get (Packet.Fields, AMI.Parser.Position, Buffer) then
         Position := Integer'Value (To_String (Buffer));
      end if;

      if Try_Get (Packet.Fields, AMI.Parser.Queue, Buffer) then
         Queue := Buffer;
      end if;

      if Try_Get (Packet.Fields, AMI.Parser.OriginalPosition, Buffer) then
         Original_Position := Integer'Value (To_String (Buffer));
      end if;

      if Try_Get (Packet.Fields, AMI.Parser.HoldTime, Buffer) then
         Hold_Time := Integer'Value (To_String (Buffer));
      end if;

      System_Messages.Notify (Debug, "My.Callbacks.Queue_Abandon: Call_ID " &
                                To_String (Call.ID) & " left queue " &
                                To_String (Queue) & " after" & Hold_Time'Img &
                                " seconds. Position:" & Position'Img & "," &
                                " original position" & Original_Position'Img);
   end Queue_Abandon;

end My_Callbacks;
