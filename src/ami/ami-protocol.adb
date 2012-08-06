with Ada.Characters.Latin_1,
     Ada.Strings.Fixed;
package body AMI.Protocol is
   --  LF is never enough for some people..
   package Char renames Ada.Characters.Latin_1;
   Line_Termination_String : constant String := Char.CR & Char.LF;

   --  Value part of request string
   Bridge_String           : constant String := "Bridge";
   CoreSettings_String     : constant String := "CoreSettings";
   Login_String            : constant String := "Login";
   Logoff_String           : constant String := "Logoff";
   Hangup_String           : constant String := "Hangup";
   Park_String             : constant String := "Park";
   Ping_String             : constant String := "Ping";
   QueueStatus_String      : constant String := "QueueStatus";
   QueuePause_String       : constant String := "QueuePause";
   Redirect_String         : constant String := "Redirect";

   --  Key part of request string
   Action_String           : constant String := "Action: ";
   ActionID_String         : constant String := "ActionID: ";
   Channel_String          : constant String := "Channel: ";
   Channel1_String         : constant String := "Channel1: ";
   Channel2_String         : constant String := "Channel2: ";
   Context_String          : constant String := "Context: ";
   Exten_String            : constant String := "Exten: ";
   Interface_String        : constant String := "Interface: ";
   Paused_String           : constant String := "Paused: ";
   Priority_String         : constant String := "Priority: ";
   Secret_String           : constant String := "Secret: ";
   Username_String         : constant String := "Username: ";

   Paused : constant array (Pause_States) of String (1 .. 1)
     := (Pause => "0", UnPause => "1");

   --  Action: Bridge
   --  [ActionID:] <value>
   --  Channel1: <value>
   --  Channel2: <value>
   --  [Tone:] <value>
   --
   --  Response
   --  Response: Success
   --  Message: Launched bridge thread with success
   function Bridge (Channel1 : in String;
                    Channel2 : in String) return String is
   begin
      return Action_String & Bridge_String & Line_Termination_String &
            Channel1_String & Channel1 & Line_Termination_String &
            Channel2_String & Channel2 & Line_Termination_String &
            Line_Termination_String;
   end Bridge;

   function CoreSettings return String is
   begin
      return Action_String & CoreSettings_String &
                  Line_Termination_String &
                  Line_Termination_String;
   end CoreSettings;

   function Hangup (Channel : in String) return String is
   begin
      return Action_String & Hangup_String & Line_Termination_String &
        Channel_String & Channel & Line_Termination_String &
        Line_Termination_String;
   end Hangup;

   function Login (Username : in String;
                   Secret   : in String) return String is
   begin
      return Action_String & Login_String & Line_Termination_String &
             Username_String & Username & Line_Termination_String &
             Secret_String & Secret & Line_Termination_String &
             Line_Termination_String;
   end Login;

   function Logoff return String is
   begin
      return Action_String & Logoff_String & Line_Termination_String &
             Line_Termination_String;
   end Logoff;

   function Park (Channel : in String;
                  Fallback_Channel : in String) return String is
   begin
      return Action_String & Park_String & Line_Termination_String &
        Channel_String & Channel & Line_Termination_String &
        Channel2_String & Fallback_Channel & Line_Termination_String &
        Line_Termination_String;
   end Park;

   function Ping return String is
   begin
      return Action_String & Ping_String & Line_Termination_String &
             Line_Termination_String;
   end Ping;

   --  Action: QueuePause
   --  Interface: SIP/1001
   --  Paused: 1
   function QueuePause (DeviceName : in String;
                        State      : in Pause_States) return String is
   begin
      return Action_String & QueuePause_String & Line_Termination_String &
        Interface_String & DeviceName & Line_Termination_String &
        Paused_String & Paused (State) & Line_Termination_String &
        Line_Termination_String;
   end QueuePause;

   function QueueStatus (ActionID : in String := "") return String is
   begin
      if ActionID'Length = 0 then
         return Action_String & QueueStatus_String & Line_Termination_String &
           Line_Termination_String;
      else
         return Action_String & QueueStatus_String & Line_Termination_String &
           ActionID_String & ActionID & Line_Termination_String &
           Line_Termination_String;
      end if;
   end QueueStatus;

   --  Action: Redirect
   --  Channel: <Channel>
   --  Context: localset
   --  Exten: 101
   --  Priority: 1
   function Redirect (Channel  : in String;
                      Context  : in String;
                      Exten    : in String;
                      Priority : in Integer := 1) return String is
      Priority_To_String : constant String := Ada.Strings.Fixed.Trim
        (Integer'Image (Priority), Ada.Strings.Left);
   begin
      return Action_String & Redirect_String & Line_Termination_String &
        Channel_String & Channel & Line_Termination_String &
        Exten_String & Exten & Line_Termination_String &
        Context_String & Context & Line_Termination_String &
        Priority_String & Priority_To_String &
        Line_Termination_String &
        Line_Termination_String;
   end Redirect;
end AMI.Protocol;
