with System_Messages;
with Ada.Calendar;
with Ada.Strings.Unbounded;
with Ada.Exceptions;

--with AMI.Protocol;
with Call_List;
with Peers;
with Notifications;
with Common;

with Event_JSON;

package body My_Callbacks is
   use System_Messages;
   use Ada.Strings.Unbounded;
   use Call_List;
   use Ada.Exceptions;
   --   use AMI.IO;
   use Peers;
   --   use Yolk.Log;

   function Current_Time return Ada.Calendar.Time renames Ada.Calendar.Clock;

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
      use Peers.Peer_List_Type;
      use Ada.Strings.Unbounded;

      --  Extracts the channel type, and the phonename,
      --    and saves them in the peer. Format: ChannelType/phonename
      procedure Set_PhoneInfo (Peer : in out Peer_Type;
                               Text : in Unbounded_String)
      is
         Seperator_Index : Integer;
      begin
         if Ada.Strings.Unbounded.Count (Text, "/") > 0 then
            Seperator_Index := Index (Text, "/");
            Peer.Peer := Tail (Source => Text,
                               Count  => Length (Text) - Seperator_Index);
            Peer.ChannelType := Head (Text, Seperator_Index - 1);
            if To_String (Peer.ChannelType) /= "SIP" then
               System_Messages.Notify (Information, To_String (Peer.ChannelType));
            end if;
         else
            System_Messages.Notify 
	      (Debug,
	       "Set_PhoneInfo:" &
		 "This peer does not have a Channeltype: "
		 & To_String (Text));
         end if;
      end Set_PhoneInfo;

      Peer      : Peer_Type;
      Map_Key   : Unbounded_String;
      Buffer    : Unbounded_String;
   begin

      System_Messages.Notify(Debug,"My_Callbacks.Peer_Status");

      if Try_Get (Packet.Fields, AMI.Parser.Peer, Buffer) then
	 Set_PhoneInfo (Peer, Buffer);
	 Map_Key := Peer.Peer;
      end if;

      -- Update fields
      Peer.Last_Seen := Current_Time;
      if Try_Get (Packet.Fields, AMI.Parser.Address, Buffer) then
         Peer.Address := Packet.Fields.Element(Address);
      end if;
      
      if Try_Get (Packet.Fields, AMI.Parser.Port, Buffer) then
         Peer.Port := Packet.Fields.Element(Port);
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
                 Image(Peer));
         end if;

         System_Messages.Notify
           (Debug, "My_Callbacks.Peer_Status: Inserted " & Image(Peer));

         Peers.Insert_Peer (New_Item => Peer);
      else
         System_Messages.Notify
           (Error, "My_Callbacks.Peer_Status: No state information supplied " &
              Image(Packet));
         raise Bad_Packet_Format;
      end if;
      
      --  Let the clients know about the change
      Notifications.Broadcast (Event_JSON.Agent_State_JSON_String (Peer) );
   end Peer_Status;
   
   --  Lists agents
   procedure Agents is
   begin
      System_Messages.Notify (Debug, "Agents not implemented");
      raise NOT_IMPLEMENTED with "Agents not implemented";
   end Agents;

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
   procedure Hangup (Packet : in Packet_Type) is
      use Ada.Strings.Unbounded;
      Call    : Call_Type;
      Call_ID : Unbounded_String;
      --  Temp_Value : Unbounded_String;
   begin
      if Try_Get (Packet.Fields, AMI.Parser.UniqueID, Call_ID) then
         Call := Call_List.Remove (Call_ID);
	 
	 if Call = Null_Call then
	    System_Messages.Notify 
	      (Information,
	       Package_Name & ".Hangup: Not in the Calls list: " &
                 " Call_ID: " & To_String (Call_ID));
	 else
	 System_Messages.Notify 
	   (Debug, Package_Name & ".Hangup: Hung up " & Image (Call) );
	    
         end if;
      else
         --  At the very least, we require an identifier for the call.
         raise BAD_PACKET_FORMAT;
      end if;
      Notifications.Broadcast(Event_JSON.Hangup_JSON_String(Call));
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
      use Ada.Strings.Unbounded;
      Call       : Call_Type := Null_Call;
      Temp_Value : Unbounded_String;
   begin
      if Try_Get (Packet.Fields, AMI.Parser.Uniqueid, Temp_Value) then
         --  The call should exsists at this point.
         Call := Call_List.Get_Call (Temp_Value);
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

      if Call.State = Call_List.Unknown then
         System_Messages.Notify
           (Debug, "My_Callbacks.Join: Call unknown state");
         Call.State := Call_List.Queued;

         if Try_Get (Packet.Fields, AMI.Parser.Queue, Temp_Value) then
            Call.Queue := Temp_Value;
         end if;

      elsif Call.State = Call_List.OnHold then
         null;
      else
         System_Messages.Notify
           (Error, "My_Callbacks.Join: Call with bad state: " &
              Call.State'Img);
      end if;
      System_Messages.Notify
        (Debug, "My_Callbacks.Join: Call Updated: " & Image(Call));
      Call_List.Update (Call);
      Notifications.Broadcast(Event_JSON.New_Call_JSON_String(Call));
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
   procedure New_Channel (Packet : in Packet_Type) is
      use Ada.Strings.Unbounded;

      Call       : Call_Type;
      Temp_Value : Unbounded_String := Null_Unbounded_String;
   begin

      if Try_Get (Packet.Fields, AMI.Parser.Channel, Temp_Value) then
         Call.Channel := Temp_Value;
      end if;

      if Try_Get (Packet.Fields, AMI.Parser.UniqueID, Temp_Value) then
         Call.Uniqueid := Temp_Value;
      end if;

      if Try_Get (Packet.Fields, AMI.Parser.Exten, Temp_Value) then
         Call.Extension := Temp_Value;
      end if;

      --  Save the time when the call came in.
      Call.Arrived := Current_Time;
      Call.State := Unknown;
      Call_List.Add (Call);
   end New_Channel;

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
   
   procedure New_State (Packet : in Packet_Type) is
   begin
      System_Messages.Notify (Debug, "My.Callbacks.New_State not implemented");
   end New_State;
   
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
      if Try_Get (Packet.Fields, AMI.Parser.UniqueID, Buffer) then
         Call.Uniqueid := Buffer;
      end if;

      if Try_Get (Packet.Fields, AMI.Parser.Position, Buffer) then
         Position := Integer'Value (To_String (Buffer) );
      end if;
      
      if Try_Get (Packet.Fields, AMI.Parser.Queue, Buffer) then
         Queue := Buffer;
      end if;
      
      if Try_Get (Packet.Fields, AMI.Parser.OriginalPosition, Buffer) then
         Original_Position := Integer'Value (To_String (Buffer) );
      end if;
      
      if Try_Get (Packet.Fields, AMI.Parser.HoldTime, Buffer) then
         Hold_Time := Integer'Value (To_String (Buffer) );
      end if;
      
      System_Messages.Notify (Debug, "My.Callbacks.Queue_Abandon: Call_ID " &
                                To_String (Call.UniqueID) & " left queue " &
                                To_String (Queue) & " after" & Hold_Time'Img &
                                " seconds. Position:" & Position'Img & "," &
                                " original position" & Original_Position'Img);
   end Queue_Abandon;
end My_Callbacks;
