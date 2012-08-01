--  with Ada.Exceptions;
with AWS.Net.Std;
with Event_Parser; use Event_Parser;
package AMI.Event is

   type Asterisk_AMI_Type is private;

   procedure Start (channel  : in AWS.Net.Std.Socket_Type;
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
      ZapShowChannelsComplete,
      --  Pragmatic observed Event, not mentioned in doc
      QueueStatusComplete
     );

   --  Basic signature of our callback routine for responses
   type Response_Callback_Type is access procedure (Event : String);
   type Callback_Type is access procedure (Event_List : Event_List_Type.Map);

   --  Callback table for actions
--     type Action_Callback_Routine_Table is array (AMI.Action.Action_Type)
--       of Callback_Type;

   --  Callback table for log events
   type Event_Callback_Routine_Table is array (Event) of Callback_Type;

   --  Callbacks
   procedure Dial_Callback              (Event_List : in Event_List_Type.Map);
   procedure Hangup_Callback            (Event_List : in Event_List_Type.Map);
   procedure Join_Callback              (Event_List : in Event_List_Type.Map);
   procedure Login_Callback             (Event_List : in Event_List_Type.Map);
   procedure PeerStatus_Callback        (Event_List : in Event_List_Type.Map);
   procedure Unlink_Callback            (Event_List : in Event_List_Type.Map);

   --  Commands
   procedure Login (Asterisk_AMI : in Asterisk_AMI_Type;
                    Username     : in String;
                    Secret       : in String);

   procedure SIPPeers_Callback;
   procedure NewState_Callback;
   procedure Bridge_Callback;
   procedure Agents;

end AMI.Event;
