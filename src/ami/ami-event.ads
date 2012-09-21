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

with Event_Parser;
with AWS.Net.Std;

package AMI.Event is
   use Event_Parser;

   type Asterisk_AMI_Type is private;

   procedure Start (Socket   : in AWS.Net.Std.Socket_Type;
                    Username : in String;
                    Secret   : in String);
   --  Here starts the part of the program, that listens for events

private
   type Asterisk_AMI_Type is
      record
         Greeting  : access String := null;
         Logged_In : Boolean := False;
         Channel   : AWS.Net.Std.Socket_Type;
      end record;

   --  The following types are derived from
   --  http://www.voip-info.org/wiki/view/asterisk+manager+events

   type Event is
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
      Shutdown,
      UserEvent,
      --  Unformatted and Undocumented
      Newstate, -- Appears when channel changes state
      ParkedCallsComplete,
      QueueParams,
      QueueMember,
      QueueStatusEnd,
      Status,
      StatusComplete,
      ZapShowChannels,
      ZapShowChannelsComplete
     );

   --  Basic signature of our callback routine for responses
   type Response_Callback_Type is access procedure (Event : String);
   type Callback_Type is access procedure (Event_List : Event_List_Type.Map);

   --  Callback table for actions
   --  type Action_Callback_Routine_Table is array (AMI.Action.Action_Type)
   --    of Callback_Type;

   --  Callback table for log events
   type Event_Callback_Routine_Table is array (Event) of Callback_Type;

   --  Callbacks
   procedure Dial_Callback       (Event_List : in Event_List_Type.Map);
   procedure Hangup_Callback     (Event_List : in Event_List_Type.Map);
   procedure Join_Callback       (Event_List : in Event_List_Type.Map);
   procedure Login_Callback      (Event_List : in Event_List_Type.Map);
   procedure Newchannel_Callback (Event_List : in Event_List_Type.Map);
   procedure PeerStatus_Callback (Event_List : in Event_List_Type.Map);
   procedure Unlink_Callback     (Event_List : in Event_List_Type.Map);

   --  Commands
   procedure Login (Asterisk_AMI : in Asterisk_AMI_Type;
                    Username     : in String;
                    Secret       : in String);

   procedure SIPPeers_Callback;
   procedure NewState_Callback;
   procedure Bridge_Callback;
   --  procedure Agents;

end AMI.Event;
