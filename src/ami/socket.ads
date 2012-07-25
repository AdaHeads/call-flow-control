--  with Ada.Exceptions;
with Event_Parser; use Event_Parser;
with Peers; use Peers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with AWS.Net.Std;
with Call_Queue;
package Socket is

   NOT_IMPLEMENTED : exception;
   type Asterisk_AMI_Type is private;

   procedure Start (channel  : in AWS.Net.Std.Socket_Type;
                    Username : in String;
                    Secret   : in String);
   --  Here starts the part of the program, that listens for events

   procedure Get_Call (Uniqueid   : in     Unbounded_String;
                       Agent      : in     Unbounded_String;
                       Call       :    out Call_Queue.Call_Type);
   --  Takes a call from the call_Queue, and redirects it to the Agent.

   procedure Bridge_Call (Channel1 : in Unbounded_String;
                          Channel2 : in Unbounded_String);

   procedure Register_Agent (PhoneName   : in Unbounded_String;
                             Computer_ID : in Unbounded_String);

   procedure Consistency_Check;

   procedure TEST_StatusPrint;
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
     --Agent Status Events
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

   --  Action types
   type Action is
     (WaitEvent, --  Wait for an event to occur (Priv: <none>)
      PlayDTMF,  --  Play DTMF signal on a specific channel. (Priv: call,all)
      AgentLogoff, --  Sets an agent as no longer logged in (Priv: agent,all)
      Agents, --  Lists agents and their status (Priv: agent,all)
      QueueReset, --  Reset queue statistics (Priv: <none>)
      QueueReload, --  Reload a queue, queues,
      --    or any sub-section of a queue or queues (Priv: <none>)
      QueueRule, --  Queue Rules (Priv: <none>)
      QueuePenalty, --  Set the penalty for a queue member (Priv: agent,all)
      QueueLog, --  Adds custom entry in queue_log (Priv: agent,all)
      QueuePause, --  Makes a queue member temporarily unavailable
                  --    (Priv: agent,all)
      QueueRemove, --  Remove interface from queue. (Priv: agent,all)
      QueueAdd, --  Add interface to queue. (Priv: agent,all)
      QueueSummary, --  Queue Summary (Priv: <none>)
      QueueStatus, --  Queue Status (Priv: <none>)
      Queues, --  Queues (Priv: <none>)
      SKINNYshowline, --  Show SKINNY line (text format)
      --    (Priv: system,reporting,all)
      SKINNYlines, --  List SKINNY lines (text format)
      --    (Priv: system,reporting,all)
      SKINNYshowdevice, --  Show SKINNY device (text format)
                        --    (Priv: system,reporting,all)
      SKINNYdevices, --  List SKINNY devices (text format)
                     --    (Priv: system,reporting,all)
      MeetmeList, -- List participants in a conference (Priv: reporting,all)
      MeetmeUnmute, -- Unmute a Meetme user (Priv: call,all)
      MeetmeMute, -- Mute a Meetme user (Priv: call,all)
      IAXregistry, -- Show IAX registrations (Priv: system,reporting,all)
      IAXnetstats, -- Show IAX Netstats (Priv: system,reporting,all)
      IAXpeerlist, -- List IAX Peers (Priv: system,reporting,all)
      IAXpeers, -- List IAX Peers (Priv: system,reporting,all)
      SIPnotify, -- Send a SIP notify (Priv: system,all)
      SIPshowregistry, --  Show SIP registrations (text format)
      --    (Priv: system,reporting,all)
      SIPqualifypeer, --  Show SIP peer (text format)
      --    (Priv: system,reporting,all)
      SIPshowpeer, --  Show SIP peer (text format) (Priv: system,reporting,all)
      SIPpeers, --  List SIP peers (text format) (Priv: system,reporting,all)
      VoicemailUsersList, --  List All Voicemail User Information
      --    (Priv: call,reporting,all)
      DAHDIRestart, --  Fully Restart DAHDI channels (terminates calls)
      --    (Priv: <none>)
      DAHDIShowChannels, -- Show status DAHDI channels (Priv: <none>)
      DAHDIDNDoff, --  Toggle DAHDI channel Do Not Disturb status OFF
      --    (Priv: <none>)
      DAHDIDNDon, --  Toggle DAHDI channel Do Not Disturb status ON
                  --    (Priv: <none>)
      DAHDIDialOffhoo, --:  Dial over DAHDI channel while offhook
      --     (Priv: <none>)
      DAHDIHangup, --  Hangup DAHDI Channel (Priv: <none>)
      DAHDITransfer, --  Transfer DAHDI Channel (Priv: <none>)
      AGI, --  Add an AGI command to execute by Async AGI (Priv: agi,all)
      UnpauseMonitor, --  Unpause monitoring of a channel (Priv: call,all)
      PauseMonitor, --  Pause monitoring of a channel (Priv: call,all)
      ChangeMonitor, --  Change monitoring filename of a channel
                     --    (Priv: call,all)
      StopMonitor, --  Stop monitoring a channel (Priv: call,all)
      Monitor, --  Monitor a channel (Priv: call,all)
      JabberSend, --  Sends a message to a Jabber Client (Priv: system,all)
      DBDelTree, --  Delete DB Tree (Priv: system,all)
      DBDel, --  Delete DB Entry (Priv: system,all)
      DBPut, --  Put DB Entry (Priv: system,all)
      DBGet, --  Get DB Entry (Priv: system,reporting,all)
      Bridge, --  Bridge two channels already in the PBX (Priv: call,all)
      Park, --  Park a channel (Priv: call,all)
      ParkedCalls, --  List parked calls (Priv: <none>)
      ShowDialPlann, --  List dialplan (Priv: config,reporting,all)
      ModuleCheck, --  Check if module is loaded (Priv: system,all)
      ModuleLoad, --  Module management (Priv: system,all)
      CoreShowChannels, --  List currently active channels
                        --    (Priv: system,reporting,all)
      Reload, --  Send a reload event (Priv: system,config,all)
      CoreStatus, --  Show PBX core status variables
                  --    (Priv: system,reporting,all)
      CoreSettings, --  Show PBX core settings (version etc)
      --    (Priv: system,reporting,all)
      UserEvent, --  Send an arbitrary event (Priv: user,all)
      UpdateConfig, --  Update basic configuration (Priv: config,all)
      SendText, --  Send text message to channel (Priv: call,all)
      ListCommands, --  List available manager commands (Priv: <none>)
      MailboxCount, --  Check Mailbox Message Count (Priv: call,reporting,all)
      MailboxStatus, --  Check Mailbox (Priv: call,reporting,all)
      AbsoluteTimeout, --  Set Absolute Timeout (Priv: system,call,all)
      ExtensionState, --  Check Extension Status (Priv: call,reporting,all)
      Command, --  Execute Asterisk CLI Command (Priv: command,all)
      Originate, --  Originate Call (Priv: originate,all)
      Atxfer, --  Attended transfer (Priv: call,all)
      Redirect, --  Redirect (transfer) a call (Priv: call,all)
      ListCategories, --  List categories in configuration file
      --    (Priv: config,all)
      CreateConfig, --  Creates an empty file in the configuration directory
      --    (Priv: config,all)
      Status, --  Lists channel status (Priv: system,call,reporting,all)
      GetConfigJSON, --  Retrieve configuration (JSON format)
                     --    (Priv: system,config,all)
      GetConfig, --  Retrieve configuration (Priv: system,config,all)
      Getvar, --  Gets a Channel Variable (Priv: call,reporting,all)
      Setvar, --  Set Channel Variable (Priv: call,all)
      Ping, --  Keepalive command (Priv: <none>)
      Hangup, --  Hangup Channel (Priv: system,call,all)
      Challenge, --  Generate Challenge for MD5 Auth (Priv: <none>)
      Login, --  Login Manager (Priv: <none>)
      Logoff, --  Logoff Manager (Priv: <none>)
      Events, --  Control Event Flow (Priv: <none>)
      None); --  Internal;

   --  Basic signature of our callback routine for responses
   type Response_Callback_Type is access procedure (Event : String);
   type Callback_Type is access procedure (Event_List : Event_List_Type);

   --  Callback table for actions
   type Action_Callback_Routine_Table is array (Action) of Callback_Type;

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
   procedure Bridge (AMI      : in Asterisk_AMI_Type;
                     ChannelA : in Unbounded_String;
                     ChannelB : in Unbounded_String);
   procedure Get_Version (AMI : in Asterisk_AMI_Type);

   procedure QueuePause (Asterisk_AMI : in Asterisk_AMI_Type;
                         Peer         : in Peer_Type);
   procedure QueueUnpause (Asterisk_AMI : in Asterisk_AMI_Type;
                           Peer         : in Peer_Type);
   procedure QueueStatus (Asterisk_AMI : in Asterisk_AMI_Type;
                          ActionID     : in String := "");
   procedure Redirect (Asterisk_AMI : in Asterisk_AMI_Type;
                       Channel      : in Unbounded_String;
                       Exten        : in Unbounded_String;
                       Context      : in Unbounded_String :=
                         To_Unbounded_String ("LocalSets"));

   procedure Ping (Asterisk_AMI : in Asterisk_AMI_Type);
   procedure Logoff (AMI        : in Asterisk_AMI_Type;
                     Callback   : access Callback_Type := null);
   procedure Login (AMI      : in Asterisk_AMI_Type;
                    Username : in String;
                    Secret   : in String;
                    Callback : in Callback_Type := null;
                    Persist  : in Boolean       := True);

   procedure SIPPeers_Callback;
   procedure NewState_Callback;
   procedure Bridge_Callback;
   procedure Agents;

   procedure SendCommand (Socket : in AWS.Net.Std.Socket_Type;
                          Item : in String);
end Socket;
