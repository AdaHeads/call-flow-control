--  with Ada.Exceptions;
with Event_Parser; use Event_Parser;
with Peers; use Peers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with AWS.Net.Std;
with Call_Queue;
with AMI.Action;
package AMI.Event is

   NOT_IMPLEMENTED : exception;
   type Asterisk_AMI_Type is private;

   procedure Start (channel  : in AWS.Net.Std.Socket_Type;
                    Username : in String;
                    Secret   : in String);
   --  Here starts the part of the program, that listens for events

   --------------------------------------------------------
   --  Should be in another file.
   procedure Get_Call (Uniqueid   : in     String;
                       Agent      : in     String;
                       Call       :    out Call_Queue.Call_Type);
   --  Takes a call from the call_Queue, and redirects it to the Agent.

   procedure Bridge_Call (Channel1 : in Unbounded_String;
                          Channel2 : in Unbounded_String);

   procedure Park (Agent : in Unbounded_String);

   procedure Register_Agent (PhoneName   : in Unbounded_String;
                             Computer_ID : in Unbounded_String);

   procedure Consistency_Check;

   procedure TEST_StatusPrint;
   ---------------------------------------------------------
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
      QueueEntry,
      QueueStatusComplete
     );

   --  Basic signature of our callback routine for responses
   type Response_Callback_Type is access procedure (Event : String);
   type Callback_Type is access procedure (Event_List : Event_List_Type);

   --  Callback table for actions
   type Action_Callback_Routine_Table is array (AMI.Action.Action_Type)
     of Callback_Type;

   --  Callback table for log events
   type Event_Callback_Routine_Table is array (Event) of Callback_Type;

   --  Callbacks
   procedure CoreSettings_Callback      (Event_List : in Event_List_Type);
   procedure Dial_Callback              (Event_List : in Event_List_Type);
   procedure Hangup_Callback            (Event_List : in Event_List_Type);
   procedure Join_Callback              (Event_List : in Event_List_Type);
   procedure Login_Callback             (Event_List : in Event_List_Type);
   procedure PeerStatus_Callback        (Event_List : in Event_List_Type);
   procedure QueueMemberPaused_Callback (Event_List : in Event_List_Type);
   procedure Unlink_Callback            (Event_List : in Event_List_Type);
   procedure QueueStatus_Callback       (Event_List : in Event_List_Type);

   procedure QueueEntry_Callback          (Event_List : in Event_List_Type);
   procedure QueueStatusComplete_CallBack (Event_List : in Event_List_Type);
   --  Commands

   procedure Get_Version (Asterisk_AMI : in Asterisk_AMI_Type);

   procedure QueuePause (Asterisk_AMI : in Asterisk_AMI_Type;
                         Peer         : in Peer_Type);
   procedure QueueUnpause (Asterisk_AMI : in Asterisk_AMI_Type;
                           Peer         : in Peer_Type);

   procedure Redirect (Asterisk_AMI : in Asterisk_AMI_Type;
                       Channel      : in Unbounded_String;
                       Exten        : in Unbounded_String;
                       Context      : in Unbounded_String :=
                         To_Unbounded_String ("LocalSets"));

   procedure Logoff (Asterisk_AMI : in     Asterisk_AMI_Type;
                     Callback     : access Callback_Type := null);
   procedure Login (Asterisk_AMI : in Asterisk_AMI_Type;
                    Username     : in String;
                    Secret       : in String;
                    Callback     : in Callback_Type := null;
                    Persist      : in Boolean       := True);

   procedure SIPPeers_Callback;
   procedure NewState_Callback;
   procedure Bridge_Callback;
   procedure Agents;

end AMI.Event;
