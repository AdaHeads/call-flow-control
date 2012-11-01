-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                AMI.Event                                  --
--                                                                           --
--                                  SPEC                                     --
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
with AMI.Parser;

package AMI.Event is
   use AMI.Parser;
   
   --  The following types are derived from
   --  http://www.voip-info.org/wiki/view/asterisk+manager+events

   type 
     Event_Type is
     --  Agent Status Events
     (Agentcallbacklogin,
      Agentcallbacklogoff,
      AgentCalled,
      AgentComplete,
      AgentConnect,
      AgentDump,
      Agentlogin,
      Agentlogoff,
      QueueMemberAdded,
      QueueMemberPaused,
      QueueMemberStatus,
      --  Command Status Events
      Cdr,
      Dial,
      ExtensionStatus,
      FullyBooted,
      Hangup,
      MusicOnHold,
      Join,
      Leave,
      Link,
      MeetmeJoin,
      MeetmeLeave,
      MeetmeStopTalking,
      MeetmeTalking,
      MessageWaiting,
      Newcallerid,
      Newchannel,
      Newexten,
      ParkedCall,
      Rename,
      SetCDRUserField,
      Unlink,
      UnParkedCall,
      --  Log Status Events
      Alarm,
      AlarmClear,
      DNDState,
      LogChannel,
      PeerStatus,
      Registry,
      Reload,
      RTCPReceived,
      RTPReceiverStat,
      RTPSenderStat,
      RTCPSent,
      Shutdown,
      UserEvent,
      --  Unformatted and Undocumented
      Newstate, -- Appears when channel changes state
      ParkedCallsComplete,
      QueueCallerAbandon,
      QueueParams,
      QueueMember,
      QueueStatusEnd,
      Status,
      StatusComplete,
      VarSet,
      ZapShowChannels,
      ZapShowChannelsComplete
     );
   
   type Event_Callback is access procedure (Packet : in Packet_Type);
   --  Prototype for an event handler. Every handler must implement this signature
   
   type Event_Callback_Table is array (Event_Type) of Event_Callback;
   --  Lookup table for event handlers
   
   procedure Null_Callback (Packet : in AMI.Parser.Packet_Type);
   --  Does nothing. Provides the ability to mute events.
   
   procedure Dispatch (Callback_Table : in Event_Callback_Table;
		       Packet         : in Packet_Type);
   
   Null_Callback_Table : constant Event_Callback_Table := 
     (others => AMI.Event.Null_Callback'access);
   
end AMI.Event;
