with System_Messages;
with Ada.Calendar;
with Ada.Strings.Unbounded;
with Ada.Exceptions;


--with AMI.IO;
--with AMI.Protocol;
with Call_List;
--with Errors;
with Peers;
--with Yolk.Log;

package body My_Callbacks is
   use System_Messages;
   --  use Ada.Strings.Unbounded;
   use Call_List;
   use Ada.Exceptions;
   --   use AMI.IO;
   use Peers;
   --   use Yolk.Log;
   --  Event: PeerStatus
   --  Peer: SIP/2005
   --  PeerStatus: Registered
   procedure Peer_Status (Packet : in Packet_Type) is
      use Peers.Peer_List_Type;
      use Ada.Strings.Unbounded;
      Peer    : Peer_Type;
      Map_Key : Unbounded_String;

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

      Temp_Value : Unbounded_String;
   begin
      
      System_Messages.Notify(Debug,"My_Callbacks.Peerstatus");
      
      if Try_Get (Packet.Fields, AMI.Parser.Peer, Temp_Value) then
	 Set_PhoneInfo (Peer, Temp_Value);
	 Map_Key := Peer.Peer;
      end if;

      declare
         Temp_Peer : Peer_Type;
      begin
         Temp_Peer := Peers.Get_Peer_By_PhoneName (Map_Key);

         if Temp_Peer /= Peers.Null_Peer then
            --  Update the timestamp
            Peer.Last_Seen := Ada.Calendar.Clock;
            Peer := Temp_Peer;
            Peers.Replace_Peer (Item => Peer);
            return;

         else
            System_Messages.Notify (Debug, "Peer not found: [" & To_String (Map_Key) & "]");
            System_Messages.Notify (Debug, Peers.List_As_String);
         end if;
      exception
         when E : others =>
            System_Messages.Notify 
	      (Debug, Exception_Name (E) & "|:|" &
		 Exception_Message (E));
      end;

      if Try_Get (Packet.Fields, AMI.Parser.Address, Temp_Value) then
	 Peer.Address := Packet.Fields.Element(Address);
      end if;

      if Try_Get (Packet.Fields, AMI.Parser.Port, Temp_Value) then
	 Peer.Port := Packet.Fields.Element(Port);
      end if;

      --  Setting the State - Registrated or not.
      if Try_Get (Packet.Fields, AMI.Parser.PeerStatus, Temp_Value) then
	 if To_String (Temp_Value) = "Unregistered"  then
	    Peer.Status := Unregistered;
	    
	    System_Messages.Notify 
	      (Debug,
	       "Got peer: " & To_String (Peer.Peer) &
		 " with unregistrered status");
	    
	 elsif To_String (Temp_Value) = "Registered" then
	    Peer.Status := Registered;
	    
	    System_Messages.Notify 
	      (Debug,
	       "Got peer: " & To_String (Peer.Peer) &
		 " with registrered status");
	    
	 else
	    System_Messages.Notify 
	      (Debug, "Peer Status, unknown state: " &
		 To_String (Temp_Value));
	 end if;
      else
	 System_Messages.Notify 
	   (Information,
	    "PeerStatus_CallbackHandler had no PeerStatus");
      end if;
      --  Gathering Extension.
      declare
	 Exten : Unbounded_String;
      begin
	 Exten := Peers.Get_Exten (Peer.Peer);
	 if Exten = Null_Unbounded_String then
	    System_Messages.Notify (Warning,
				    "There is not registrated any extension to agent: "
				      & To_String (Peer.Peer));
	    raise Program_Error;
	 else
	    Peer.Exten := Exten;
	 end if;
      end;
      --  Insert Timestamp
      Peer.Last_Seen := Ada.Calendar.Clock;
      Peers.Insert_Peer (Peer);
      System_Messages.Notify(Debug,List_As_String);
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
	       "Got a hangup on a call, that was not in the Calls list." &
                 " Uniqueid: " & To_String (Call_ID));
	 else
	 System_Messages.Notify 
	   (Debug, "This call have been hangup: " &
	      "Channel: " & To_String (Call.Channel) &
	      "UniqueID: " & To_String (Call.Uniqueid));
	    
         end if;
      else
	 System_Messages.Notify
	   (Information,
	    "Got a hangup on a call, that was not in the Calls list." &
	      " Uniqueid: " & To_String (Call_ID));
      end if;
      
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
	       "My_Callbacks.Join: Got a Join Event, on a call that don't exsist" &
		 " and do not have a Channel");
         end if;
         return;
      end if;

      if Call.State = Call_List.Unknown then
         System_Messages.Notify (Debug, "My_Callbacks.Join: Call unknown state");
         Call.State := Call_List.Queued;

         if Try_Get (Packet.Fields, AMI.Parser.Queue, Temp_Value) then
            Call.Queue := Temp_Value;
         end if;

      elsif Call.State = Call_List.OnHold then
         null;
      else
         System_Messages.Notify (Error, "My_Callbacks.Join: Call with bad state: " &
      				   Call.State'Img);
      end if;
      System_Messages.Notify (Debug, "My_Callbacks.Join: Call Updated: " & Image(Call));
      Call_List.Update (Call);
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
      Call.Arrived := Ada.Calendar.Clock;
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
   
   procedure New_State  (Packet : in Packet_Type) is
   begin
      System_Messages.Notify (Debug, "My.Callbacks.New_State not implemented");
   end New_State;
   
end My_Callbacks;
