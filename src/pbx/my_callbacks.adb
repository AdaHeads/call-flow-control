-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                              My_Callbacks                                 --
--                                                                           --
--                                  BODY                                     --
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

with Model.Call;
with Model.Peers;
with Handlers.Notifications;

with JSON.Event;

package body My_Callbacks is
   use Common;
   use System_Messages;
   use Ada.Strings.Unbounded;
   use Model.Call;
   use Model.Peers;

   package Notifications renames Handlers.Notifications;

   --------------
   --  Agents  --
   --------------

   procedure Agents is
   begin
      System_Messages.Notify (Debug, "Agents not implemented");
      raise NOT_IMPLEMENTED with "Agents not implemented";
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
      raise NOT_IMPLEMENTED with "Bridge not implemented";
   end Bridge;

   --  Event: Dial
   --  Privilege: call,all
   --  SubEvent: Begin
   --  Channel: SIP/softphone2-0000000a
   --  Destination: SIP/softphone1-0000000b
   --  CallerIDNum: softphone2
   --  CallerIDName: <unknown>
   --  UniqueID: 1340097427.10
   --  DestUniqueID: 1340097427.11
   --  Dialstring: softphone1
   procedure Dial (Packet : in Packet_Type) is
      --  use Ada.Strings.Unbonded;
   --  Temp_Value : Unbounded_String;
   pragma Unreferenced (Packet);
   begin
      System_Messages.Notify (Debug, "Dial not implemented");
      raise NOT_IMPLEMENTED with "Dial not implemented";
   end Dial;

   --  Event: Hangup
   --  Privilege: call,all
   --  Channel: SIP/softphone1-0000000b
   --  Uniqueid: 1340097427.11
   --  CallerIDNum: 100
   --  CallerIDName: <unknown>
   --  Cause: 16
   --  Cause-txt: Normal Clearing
   procedure Hangup (Packet : in Packet_Type)
   is
      Call    : Call_Type;
      Call_ID : Unbounded_String;
      --  Temp_Value : Unbounded_String;
   begin
      if Try_Get (Packet.Fields, AMI.Parser.Uniqueid, Call_ID) then
         Call := Remove (Call_ID);

         if Call = Null_Call then
            System_Messages.Notify
              (Information,
               Package_Name & ".Hangup: Not in the Calls list: " &
                 " Call_ID: " & To_String (Call_ID));
         else
            System_Messages.Notify
              (Debug, Package_Name & ".Hangup: Hung up " & Image (Call));

         end if;
      else
         --  At the very least, we require an identifier for the call.
         raise BAD_PACKET_FORMAT;
      end if;
      Notifications.Broadcast (JSON.Event.Hangup_JSON_String (Call));
   end Hangup;

   --  Event: Join
   --  Privilege: call,all
   --  Channel: SIP/TP-Softphone-00000036
   --  CallerIDNum: TP-Softphone
   --  CallerIDName: unknown
   --  Queue: org_id1
   --  Position: 1
   --  Count: 1
   --  Uniqueid: 1347875403.105
   procedure Join (Packet : in Packet_Type) is
      Call       : Call_Type := Null_Call;
      Temp_Value : Unbounded_String;
   begin
      if Try_Get (Packet.Fields, AMI.Parser.Uniqueid, Temp_Value) then
         --  The call should exsists at this point.
         Call := Get_Call (Temp_Value);
      end if;

      --  There are no call with that ID, something is wrong.
      if Call = Null_Call then
         if Try_Get (Packet.Fields, AMI.Parser.Channel, Temp_Value) then
            System_Messages.Notify
              (Error,
               "My_Callbacks.Join: Got a Join event, " &
                 "on a call there is not in the call list. " &
                 "Channel: " &
                 To_String (Temp_Value));
         else
            System_Messages.Notify
              (Error,
               "My_Callbacks.Join: Got a Join Event, on a call that " &
                 "don't exsist and do not have a Channel");
         end if;

         return;
      end if;

      if Call.State = Unknown then
         System_Messages.Notify
           (Debug, "My_Callbacks.Join: Call unknown state");
         Call.State := Queued;

         if Try_Get (Packet.Fields, AMI.Parser.Queue, Temp_Value) then
            Call.Queue := Temp_Value;
         end if;

      elsif Call.State = OnHold then
         null;
      else
         System_Messages.Notify
           (Error, "My_Callbacks.Join: Call with bad state: " &
              Call.State'Img);
      end if;
      System_Messages.Notify
        (Debug, "My_Callbacks.Join: Call Updated: " & Image (Call));
      Update (Call);
      Notifications.Broadcast (JSON.Event.New_Call_JSON_String (Call));
   end Join;

   --  Event: Newchannel
   --  Privilege: call,all
   --  Channel: SIP/TP-Softphone-00000036
   --  ChannelState: 0
   --  ChannelStateDesc: Down
   --  CallerIDNum: TP-Softphone
   --  CallerIDName:
   --  AccountCode:
   --  Exten: 7001
   --  Context: LocalSets
   --  Uniqueid: 1347875403.105
   procedure New_Channel
     (Packet : in Packet_Type)
   is
      Call       : Call_Type;
      Temp_Value : Unbounded_String := Null_Unbounded_String;
   begin

      if Try_Get (Packet.Fields, AMI.Parser.Channel, Temp_Value) then
         Call.Channel := Temp_Value;
      end if;

      if Try_Get (Packet.Fields, AMI.Parser.Uniqueid, Temp_Value) then
         Call.Uniqueid := Temp_Value;
      end if;

      if Try_Get (Packet.Fields, AMI.Parser.Exten, Temp_Value) then
         Call.Extension := Temp_Value;
      end if;

      --  Save the time when the call came in.
      Call.Arrived := Current_Time;
      Call.State := Unknown;
      Add (Call);
   end New_Channel;

   procedure New_State
     (Packet : in Packet_Type)
   is
      pragma Unreferenced (Packet);
   begin
      System_Messages.Notify (Debug, "My.Callbacks.New_State not implemented");
   end New_State;

   --  Event: Newstate
   --  Privilege: call,all
   --  Channel: SIP/softphone1-0000000b
   --  ChannelState: 5
   --  ChannelStateDesc: Ringing
   --  CallerIDNum: 100
   --  CallerIDName:
   --  Uniqueid: 1340097427.11
   procedure NewState is
   begin
      System_Messages.Notify (Debug, "NewState_Callback not implemented");
   end NewState;

   --  Event: PeerStatus
   --  Privilege: system,all
   --  ChannelType: SIP
   --  Peer: SIP/softphone1
   --  PeerStatus: Registered
   --  Address: 192.168.2.142
   --  Port: 5060

   --  Event: PeerStatus
   --  Privilege: system,all
   --  ChannelType: SIP
   --  Peer: SIP/softphone1
   --  PeerStatus: Unregistered
   procedure Peer_Status (Packet : in Packet_Type) is
      use Peer_List_Type;

      procedure Set_PhoneInfo
        (Peer : in out Peer_Type;
         Text : in     Unbounded_String);
      --  Extracts the channel type, and the phonename,
      --    and saves them in the peer. Format: ChannelType/phonename

      ---------------------
      --  Set_PhoneInfo  --
      ---------------------

      procedure Set_PhoneInfo
        (Peer : in out Peer_Type;
         Text : in     Unbounded_String)
      is
         Seperator_Index : Integer;
      begin
         if Ada.Strings.Unbounded.Count (Text, "/") > 0 then
            Seperator_Index := Index (Text, "/");
            Peer.Peer := Tail (Source => Text,
                               Count  => Length (Text) - Seperator_Index);
            Peer.ChannelType := Head (Text, Seperator_Index - 1);
            if To_String (Peer.ChannelType) /= "SIP" then
               System_Messages.Notify
                 (Information, To_String (Peer.ChannelType));
            end if;
         else
            System_Messages.Notify
              (Debug,
               "Set_PhoneInfo:" &
                 "This peer does not have a Channeltype: "
               & To_String (Text));
         end if;
      end Set_PhoneInfo;

      Peer    : Peer_Type;
      --  Map_Key : Unbounded_String;
      Buffer  : Unbounded_String;
   begin

      System_Messages.Notify (Debug, "My_Callbacks.Peer_Status");

      if Try_Get (Packet.Fields, AMI.Parser.Peer, Buffer) then
         Set_PhoneInfo (Peer, Buffer);
         --  Map_Key := Peer.Peer;
      end if;

      --  Update fields
      Peer.Last_Seen := Current_Time;
      if Try_Get (Packet.Fields, AMI.Parser.Address, Buffer) then
         Peer.Address := Packet.Fields.Element (Address);
      end if;

      if Try_Get (Packet.Fields, AMI.Parser.Port, Buffer) then
         Peer.Port := Packet.Fields.Element (Port);
      end if;

      --  Setting the State - Registrated or not.
      if Try_Get (Packet.Fields, AMI.Parser.PeerStatus, Buffer) then
         --  Save the previous state.
         Peer.Last_State := Peer.State;
         if To_String (Buffer) = "Unregistered"  then
            Peer.State := Unregistered;

            System_Messages.Notify
              (Debug,
               "Got peer: " & To_String (Peer.Peer) &
                 " with unregistrered status");

         elsif To_String (Buffer) = "Registered" then
            Peer.State := Idle;
         else
            Peer.State := Unknown;
            System_Messages.Notify
              (Error, "My_Callbacks.Peer_Status: Peer in unknown state " &
                 Image (Peer));
         end if;

         System_Messages.Notify
           (Debug, "My_Callbacks.Peer_Status: Inserted " & Image (Peer));

         Insert_Peer (New_Item => Peer);
      else
         System_Messages.Notify
           (Error, "My_Callbacks.Peer_Status: No state information supplied " &
              Image (Packet));
         raise BAD_PACKET_FORMAT;
      end if;

      --  Let the clients know about the change
      Notifications.Broadcast (JSON.Event.Agent_State_JSON_String (Peer));
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
         Call.Uniqueid := Buffer;
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
                                To_String (Call.Uniqueid) & " left queue " &
                                To_String (Queue) & " after" & Hold_Time'Img &
                                " seconds. Position:" & Position'Img & "," &
                                " original position" & Original_Position'Img);
   end Queue_Abandon;

   --  Lists the SIP peers. Returns a PeerEntry event for each
   --  SIP peer, and a PeerlistComplete event upon completetion
   --  Event: PeerEntry
   --  Channeltype: SIP
   --  ObjectName: softphone2
   --  ChanObjectType: peer
   --  IPaddress: 90.184.227.68
   --  IPport: 59028
   --  Dynamic: yes
   --  Natsupport: yes
   --  VideoSupport: no
   --  TextSupport: no
   --  ACL: no
   --  Status: Unmonitored
   --  RealtimeDevice: no
   --
   --  Event: PeerlistComplete
   --  EventList: Complete
   --  ListItems: 2
   procedure SIPPeers is
   begin
      System_Messages.Notify (Debug, "SipPeers_Callback not implemented");
      raise NOT_IMPLEMENTED;
   end SIPPeers;

end My_Callbacks;
