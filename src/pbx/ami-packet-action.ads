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

--  constructors for AMI action packets. Comments are (mostly) copies of
--  the appendix F from the Asterisk book[1] and Asterisk wiki [2], which are
--  licensed under Creative Commons.
--
--  [1] http://astbook.asteriskdocs.org/en/2nd_Edition/asterisk-book-html-chunk
--  [2] https://wiki.asterisk.org/wiki/display/AST/Asterisk+10+AMI+Actions

with AMI.Packet.Field;
with AMI.Parser;

package AMI.Packet.Action is
   use AMI.Packet.Field;

   Not_Implemented : exception;

   type Valid_Authentications is (MD5);

   type Config_Action is (NewCat, RenameCat, DelCat, Update, Delete, Append);

   type Valid_Digit is (Zero, One, Two, Three, Four, Five, Six, Seven, Eight,
                        Nine, Octothorpe, Asterisk, A, B, C, D);
   type Valid_Event_Values  is (System, Call, Log);

   type Event_Masks is array (Valid_Event_Values) of Boolean;

   type Event_Mask (On : Boolean) is tagged record
      case On is
         when True =>
            Masks : Event_Masks := (others => False);
         when False =>
            null;
      end case;
   end record;

   function To_String (Mask : in Event_Mask) return String;

   type Response_Handler_Type is access
     procedure (Packet : AMI.Parser.Packet_Type);

   procedure Null_Reponse_Handler (Packet : AMI.Parser.Packet_Type) is null;

   Ignore : constant Response_Handler_Type := Null_Reponse_Handler'Access;

   type Request (Asynchronous : Boolean)
     is tagged limited private;
   --  A request and it's primities

   function Action_ID (R : in Request) return Action_ID_Type;

   function Response_Handler (R : in Request) return Response_Handler_Type;

   function To_AMI_Packet (R : in Request) return AMI_Packet;

   procedure Add_Field (R : in out Request;
                        F : in     AMI.Packet.Field.Field);
   --  Add an additional field to the request.

   Null_Request : constant Request;

   function Absolute_Timeout
     (Channel     : in  String;
      --  The Name of The Channel On Which To Set The Absolute Timeout.
      Timeout     : in     Duration := Duration'First;
      --  The maximum duration of the call, in seconds.
      On_Response : in     Response_Handler_Type := Null_Reponse_Handler'Access
      --  The reponse handler
     ) return Request;
   --  Set Absolute Timeout (Priv : system, call, all)
   --  This will hangup a specified channel after a certain number of seconds,
   --  thereby actively ending the call.
   --  Asterisk will acknowledge the timeout setting with a
   --  Timeout Set message.

   function Agent_Logoff
     (Agent       : in String;
      --  Agent ID of the agent to log off
      Soft        : in Boolean := False;
      --  Set to true to not hangup existing calls.
      On_Response : in     Response_Handler_Type := Null_Reponse_Handler'Access
      --  The response handler.
     ) return Request;
   --  Logs off the specified agent for the queue system.

   function Agents
     (On_Response : in Response_Handler_Type
      := Null_Reponse_Handler'Access
      --  The response handler.
     ) return Request;
   --  This action lists information about all configured agents.

   function AGI
     (Channel     : in String;
      --  Channel that is currently in Async AGI.
      Command     : in String;
      --  Application to execute.
      CommandID   : in String;
      --  This will be sent back in CommandID header of AsyncAGI
      --  exec event notification.
      On_Response : in Response_Handler_Type := Null_Reponse_Handler'Access
      --  The response handler.
     ) return Request;

   procedure AOCMessage
     (Channel         : in String;
      --  Channel name to generate the AOC message on.
      Channel_Prefix  : in String;
      --  Partial channel prefix.
      Message_Type    : in Character;
      --  Defines what type of AOC message to create, AOC-D or AOC-E (D,E).
      Charge_Type     : in String;
      --  Defines what kind of charge this message represents.
      --  (NA, FREE, Currency, Unit).
      Unit_Amount     : in Natural;
      --  This represents the amount of units charged
      Unit_Type       : in Positive;
      --  integer value between 1 and 16.
      Currency_Name   : in String;
      --  This value is truncated after 10 characters.
      Currency_Amount : in Positive
     ) is null;
   --  TODO add internal multiplier, and a Variant record type for the
   --  Arguments.

   function Atxfer
     (Channel   : in String;
      --  Transferer's channel.
      Extension : in String;
      --  Extension to transfer to.
      Context   : in String;
      --  Context to transfer to.
      Priority  : in Natural;
      --  Priority To Transfer To.
      On_Response : in     Response_Handler_Type := Null_Reponse_Handler'Access
      --  The response handler.
     ) return Request;
   --  Attended transfer.

   function Bridge
     (Channel1 : in String;
      Channel2 : in String;
      Tone     : in Boolean := False;
      On_Response : in     Response_Handler_Type := Null_Reponse_Handler'Access
      --  The response handler.
     ) return Request;
   --  Bridge together two channels already in the PBX.

   function Challenge
     (Authentication_Type : in Valid_Authentications := MD5;
      On_Response : in     Response_Handler_Type := Null_Reponse_Handler'Access
      --  The response handler.
     ) return Request;
   --  Generate a challenge for MD5 authentication.

   function Change_Monitor
     (Channel    : in String;
      --  Used to specify the channel to record.
      File        : in String;
      --  The new filename in which the monitored channel will be recorded.
      On_Response : in Response_Handler_Type
      := Null_Reponse_Handler'Access
      --  The response handler.
     ) return Request;
   --  The ChangeMonitor action may be used to change the file started by
   --  a previous Monitor action.

   function Command
     (In_Command     : in String;
      --  Asterisk CLI command to run.
      On_Response : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
      --  The reponse handler
     ) return Request;
   --  Runs an Asterisk CLI command as if it had been run from the CLI.

   function Core_Settings
     (On_Response : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
      --  The reponse handler
     ) return Request;
   --  Show PBX core settings (version etc).

   function Core_Show_Channels
     (On_Response : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
      --  The reponse handler
     ) return Request;
   --  List currently defined channels and some information about them.

   function Core_Status
     (On_Response : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
      --  The reponse handler
     ) return Request;
   --  Query for Core PBX status.

   function Create_Config
     (Filename    : in String;
      On_Response : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
      --  The reponse handler
     ) return Request;
   --  This action will create an empty file in the configuration directory,
   --  and is intended to be used before an UpdateConfig action.

   procedure DB_Get
     (Family : in String;
      --  The AstDB key family from which to retrieve the value.
      Key    : in String;
      --  The name of the AstDB key.
      On_Response : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
      --  The reponse handler
     ) is null;
   --  This action retrieves a value from the AstDB database.

   procedure DB_Put
     (Family     : in String;
      --  The AstDB key family from which to retrieve the value.
      Key        : in String;
      --  The name of the AstDB key.
      Value      : in String;
      --  The Value To Assign To The Key.
      On_Response : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
      --  The reponse handler
     ) is null;
   --  Sets a key value in the AstDB database.

   procedure Events
     (Mask        : in Event_Mask;
      --  Set to on if all events should be sent, off if events should not be
      --  sent, or system, call, log to select which type of events should be
      --  sent to this manager connection.
      On_Response : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
      --  The reponse handler
     ) is null;
   --  Controls event flow.
   --  Enables or disables sending of events to this manager connection.

   procedure Extension_State
     (Extension : in String;
      --  The name of the extension to check.
      Context   : in String;
      --  The name of the context that contains the extension.
      On_Response : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
      --  The reponse handler.
     ) is null;
   --  Checks extension status
   --  This command reports the extension state for the given extension.
   --  If the extension has a hint, this will report the status of the
   --  device connected to the extension.

   procedure Get_Config
     (Filename : in String;
      --  Name of the configuration file to retrieve.
      On_Response : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
      --  The reponse handler.
      ) is null;
   --  Retrieves the data from an Asterisk configuration file.

   procedure Get_Var
     (Channel : in String;
      --  The name of the channel from which to retrieve the variable value.
      Variable : in String;
      --  The name of the variable to retrieve.
      On_Response : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
      --  The reponse handler.
     ) is null;
   --  Gets the value of a local channel variable or global variable

   function Hangup
     (Channel     : in String;
      --  The channel name to be hung up.
      On_Response : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
      --  The reponse handler
     ) return Request;
   --  Hangs up the specified channel.

   --  Not implemented due to broken return format.
   procedure IAX_Net_Stats is null;
   --  Shows a summary of network statistics for the IAX2 channel driver.

   --  Not implemented due to broken return format.
   procedure IAX_Peers is null;
   --  Lists all IAX2 peers and their current status.

   --  Not implemented due to broken return format.
   procedure ListCommands is null;
   --  Lists the action name and synopsis for every Asterisk Manager
   --  Interface action.

   function Login
     (Username : in String;
      --  Username to login with as specified in manager.conf.
      Secret   : in String;
      --  Secret to login with as specified in manager.conf.
      On_Response : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
      --  The reponse handler.
     ) return Request;
   --  Login to the AMI socket. This is mandatory, and the socket will
   --  close after a timeout if login is not sent in time
   --  after the socket connection is established.

   function Logoff
     (On_Response : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
      --  The reponse handler.
     ) return Request;
   --  Logs off this manager session. This will effectuate a socket close at
   --  remote end.

   procedure Mailbox_Count
     (Mailbox     : in String;
      --  The mailbox to query.
      On_Response : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
      --  The reponse handler.
     ) is null;
   --  Retrieves the number of messages for the specified voice mailbox.

   procedure Mailbox_Status
     (Mailbox     : in String;
      --  The full mailbox ID, including mailbox and context (box context).
      On_Response : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
      --  The reponse handler.
     ) is null;
   --  Checks the status for the specified voicemail box.

   procedure Meetme_Mute
     (Meetme_Number : in String;
      --  The MeetMe conference bridge number.
      User_Number   : in String;
      --  The user number in the specified bridge.
      On_Response   : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
      --  The reponse handler.
     ) is null;
   --  Mutes a particular user in a MeetMe conference bridge.

   procedure Meetme_UnmUte
     (Meetme_Number : in String;
      --  The MeetMe conference bridge number.
      User_Number   : in String;
      --  The user number in the specified bridge.
      On_Response   : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
      --  The reponse handler.
     ) is null;
   --  Unmutes a particular user in a MeetMe conference bridge.

   procedure Monitor
     (Channel : in String;
      --  Specifes the channel to be recorded.
      File    : in String := "";
      --  The name of the file in which to record the channel.
      --  if no filename is specified, the filename will be the name of
      --  the channel, with slashes replaced with dashes.
      Format  : in String := "wav";
      --  The audio format in which to record the channel.
      Mix     : in Boolean := True;
      --  Specifying whether or not Asterisk should mix the inbound and
      --  outbound audio from the channel in to a single file.
      On_Response   : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
      --  The reponse handler.
     ) is null;
   --  Records the audio on a channel to the specified file.

   function Originate
     (Channel      : in String;
      --  Channel name to call. Once the called channel has answered, the
      --  control of the call will be passed to the specified
      --  Exten/Context/Priority.
      Extension    : in String;
      --  Extension to use (requires Context and Priority).
      Context      : in String;
      --  Context to use.
      Priority     : in Natural;
      --  Priority to use.
      Timeout      : in Duration := 30.0;
      --  How long to wait for call to be answered.
      CallerID     : in String := "";
      --  Caller ID to be set on the outgoing channel.
      Variable     : in String := "";
      --  Channel variable to set. Multiple variable headers are allowed.
      Account      : in String := "";
      --  Account code.
      Codecs       : in String := "";
      --  Comma-separated list of codecs to use for this call.
      On_Response  : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
      --  The reponse handler.
     ) return Request;
   --  Generates an outbound call from Asterisk, and connect the channel to a
   --  context/extension/priority combination.

   procedure Originate
     (Channel      : in String;
      --  Channel name to call. Once the called channel has answered, the
      --  control of the call will be passed to the specified Application.
      Application  : in String;
      --  Application to use.
      Data         : in String;
      --  Data to pass as parameters to the application.
      Timeout      : in Duration := 30.0;
      --  How long to wait for call to be answered.
      CallerID     : in String := "";
      --  Caller ID to be set on the outgoing channel.
      Variable     : in String := "";
      --  Channel variable to set. Multiple variable headers are allowed.
      Account      : in String := "";
      --  Account code.
      Codecs       : in String := "";
      --  Comma-separated list of codecs to use for this call.
      On_Response  : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
      --  The reponse handler.
     ) is null;
   --  Generates an outbound call from Asterisk, and connect the channel to a
   --  dialplan application

   function Park
     (Channel      : in String;
      --  Channel name to park.
      Channel2     : in String;
      --  Channel to announce park info to (and return the call to if the
      --  parking times out).
      Timeout      : in Duration := 45.0;
      --  Number of milliseconds to wait before callback.
      Parkinglot   : String := "";
      On_Response  : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
      --  The reponse handler.
     ) return Request;
   --  Parks the specified channel in a parking lot.

   procedure Parked_Calls
     (On_Response  : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
      --  The reponse handler.
     ) is null;
   --  Lists any calls that are parked in the call parking lot.

   procedure Pause_Monitor
     (Channel : in String;
      --  The channel identifier of the channel that is currently
      --  being monitored.
      On_Response  : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
      --  The reponse handler.
     ) is null;
   --  Pauses the monitoring (recording) of a channel if it is being monitored.

   function Ping
     (On_Response  : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
      --  The reponse handler.
     )  return Request;
   --  Queries the Asterisk server to make sure it is still responding.
   --  Asterisk will respond with a Pong response. This command can also be
   --  used to keep the manager connection from timing out.

   procedure Play_DTMF
     (Channel : in String;
      --  The identifier for the channel on which to send the DTMF digit.
      Digit   : in Valid_Digit;
      --  The DTMF digit to play on the channel.
      On_Response  : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
      --  The reponse handler.
     ) is null;

   procedure Queue_Add
     (Queue  : in String;
      --  The name of the queue.
      Interface_Name : in String;
      --  The name of the member to add to the queue. This will be a technology
      --  and resource such as SIP/Jane or Local/203@lab/n.
      MemberName : in String := "";
      --  This is a human-readable alias for the interface, and will appear in
      --  the queue statistics and queue logs.
      Penalty    : in Natural := 0;
      --  A numerical penalty to apply to this queue member. Asterisk will
      --  distribute calls to members with higher penalties only after
      --  attempting to distribute the call to all members with a lower penalty
      Paused     : in Boolean := False;
      --  Whether or not the member should be initially paused.
      On_Response  : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
      --  The reponse handler.
     ) is null;
   --  Adds a queue member to a call queue.

   procedure Queue_Pause
     (Interface_Name : in String;
      --  The name of the interface to pause or unpause.
      Paused     : in Boolean := False;
      --  Whether or not the interface should be paused. Set to true to pause
      --  the member, or false to unpause the member.
      Queue : in String := "";
      --  The name of the queue in which to pause or unpause this member.
      --  If not specified, the member will be paused or unpaused in all
      --  the queues it is a member of.
      On_Response  : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
      --  The reponse handler.
     ) is null;
   --  Pauses or unpauses a member in a call queue.

   procedure Queue_Remove
     (Queue : in String;
      --  Which queue to remove the member from.
      Interface_Name : in String;
      --  The interface (member) to remove from the specified queue
      On_Response  : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
      --  The reponse handler.
     ) is null;
   --  Removes interface from queue.

   procedure Queue_Status
     (Queue : in String := "";
      --  If specified, limits the response to the status of the
      --  specified queue.
      Member : in String := "";
      --  ??? TODO
      On_Response  : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
      --  The reponse handler.
     ) is null;
   --  Checks the status of one or more queues.

   --  Not implemented due to broken return format. Use Queue_Status.
   procedure Queues is null;
      --  Shows the call queues along with the queue members, callers,
      --  and basic queue statistics.

   function Redirect
     (Channel         : in String;
      --  The channel to redirect.
      Extra_Channel   : in String := "";
      --  Channel identifier of the second call leg to transfer.
      Extension       : in String;
      --  Extension in the dialplan to transfer to.
      Extra_Extension : in String := "";
      --  Extension to transfer extrachannel to.
      Context         : in String;
      --  Context to transfer to.
      Extra_Context   : in String := "";
      --  Context to transfer extrachannel to.
      Priority        : in Natural := 1;
      --  Priority to transfer to.
      Extra_Priority  : in Natural := Natural'Last;
      --  Priority to transfer extrachannel to.
      On_Response     : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
      --  The reponse handler.
     ) return Request;
   --  Redirects a channel to a new context, extension, and
   --  priority in the dialplan.

   function SIP_Peers
     (On_Response  : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
      --  The reponse handler.
     ) return Request;
   --  Lists the currently configured SIP peers along with their status.

   procedure SIP_Show_Peer
     (Peer : in String;
      --  The name of the SIP peer.
      On_Response  : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
      --  The reponse handler.
     ) is null;
   --  Shows detailed information about a configured SIP peer.

   procedure Set_CDR_User_Field
     (Channel     : in String;
      --  The channel on which to set the CDR UserField.
      User_Field : in String;
      --  The value to assign to the UserField in the CDR record.
      On_Response  : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
      --  The reponse handler.
     ) is null;
   --  Sets the UserField setting for the CDR record on the specified channel.

   procedure Set_Var
     (Channel : in String := "";
      --  Channel on which to set the variable. If not set, the variable will
      --  be set as a global variable.
      Variable : in String;
      --  Variable name.
      Value    : in String;
      --  Variable value;
      On_Response  : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
      --  The reponse handler.
     ) is null;
   --  Sets a global or channel variable.

   procedure Status
     (Channel : in String := "";
      --  Limits the status output to the specified channel.
      On_Response  : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
      --  The reponse handler.
     ) is null;
   --  Lists the status of one or more channels, showing details about their
   --  current state.

   procedure Stop_Monitor
     (Channel : in String;
      --  The name of the channel to stop monitoring.
      On_Response  : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
      --  The reponse handler.
     ) is null;
   --  Stops a previously started monitoring (recording) on a channel.

   procedure Unpause_Monitor
     (Channel : in String;
      --  The name of the channel on which to unpause the monitoring.
      On_Response  : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
      --  The reponse handler.
     ) is null;
   --  Unpauses the monitoring (recording) of the specified channel.

   procedure Update_Config
     (Source_Filename : in String;
      --  The filename of the configuration file from which to read the
      --  current information.
      Destination_Filename : in String;
      --  The filename of the configuration file to be written.
      Reload               : Boolean := False;
      --  Specifies whether or not a reload should take place after the
      --  configuration update, or the name of a specific module that
      --  should be reloaded.
      Action               : in Config_Action;
      --  An action to take.
      Category             :  in String;
      --  The name of the category to operate on.
      Variable             : in String := "";
      --  The name of the variable to operate on.
      Value                : in String := "";
      --  The Value of The Variable To Operate On.
      Match                : in String := "";
      --  If set, an extra parameter that must be matched on the line.
      On_Response  : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
      --  The reponse handler.
     ) is null;
   --  Dynamically updates an Asterisk configuration file.
   --  TODO: Handle the -000000 problem.

   procedure User_Event
     (User_Event : in String;
      --  The name of the arbitrary event to send.
      Header     : in String := "";
      --  The name and value of an arbitrary parameter to your event.
      --  You may add as many additional headers (along with their values)
      --  to your event.
      On_Response  : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
      --  The reponse handler.
     ) is null;
   --  Sends an arbitrary event to the Asterisk Manager Interface.

   function Voicemail_Users_List
     (On_Response  : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
      --  The reponse handler.
     ) return Request;
   --  List All Voicemail User Information.

   function Wait_Event
     (Timeout : in Duration := 30.0;
      --  Maximum time to wait for events.
      On_Response  : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
      --  The reponse handler.
     ) return Request;
      --  Waits for an event to occur.
      --  After calling this action, Asterisk will send you a Success response
      --  as soon as another event is queued by the Asterisk Manager Interface.
      --  Once WaitEvent has been called on an HTTP manager session,
      --  events will be generated and queued.
private
   type Valid_Action is (Undefined,
                         AGI,
                         AgentCallbackLogin,
                         AgentLogoff,
                         Agents,
                         AbsoluteTimeout,
                         Atxfer,
                         Bridge,
                         Challenge,
                         ChangeMonitor,
                         Command,
                         CoreSettings,
                         CoreShowChannels,
                         CoreStatus,
                         CreateConfig,
                         DAHDIDialOffHook,
                         Hangup,
                         Login,
                         Logoff,
                         Originate,
                         Park,
                         Ping,
                         Redirect,
                         SIPPeers,
                         VoicemailUsersList,
                         WaitEvent);

   Digit_Value : constant array (Valid_Digit) of Character :=
                   (Zero       => '0',
                    One        => '1',
                    Two        => '2',
                    Three      => '3',
                    Four       => '4',
                    Five       => '5',
                    Six        => '6',
                    Seven      => '7',
                    Eight      => '8',
                    Nine       => '9',
                    Octothorpe => '#',
                    Asterisk   => '*',
                    A          => 'A',
                    B          => 'B',
                    C          => 'C',
                    D          => 'D');

   type Request (Asynchronous : Boolean) is tagged limited
      record
         Action           : Valid_Action;
         Action_ID        : Action_ID_Type := Next;
         Fields           : Field_List.List;
         Response_Handler : Response_Handler_Type;
      end record;

   Null_Request : constant Request :=
                    (Asynchronous     => True,
                     Action           => Undefined,
                     Action_ID        => Null_Action_ID,
                     Fields           => Field_List.Empty_List,
                     Response_Handler => Null_Reponse_Handler'Access);

end AMI.Packet.Action;
