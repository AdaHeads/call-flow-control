--  with AMI.Event; use AMI.Event;
with AMI.IO;
with Ada.Text_IO;
package body AMI.Action is

   Last_Action      : Action_Type := None;

   procedure Bridge (Socket   : in Socket_Type;
                     ChannelA : in String;
                     ChannelB : in String) is
      Command : constant String := Protocol.Bridge (ChannelA, ChannelB);
   begin
      AMI.IO.Send (Socket => Socket,
                   Item   => Command);
   end Bridge;

   procedure CoreSettings (Socket : in Socket_Type) is
      Command : constant String := Protocol.CoreSettings;
   begin
      AMI.IO.Send (Socket, Command);
      Last_Action := CoreSettings;
   end CoreSettings;

   function Get_Last_Action return Action_Type is
   begin
      return Last_Action;
   end Get_Last_Action;

   procedure Login (Socket   : in Socket_Type;
                    Username : in String;
                    Secret   : in String) is
      Command : constant String := Protocol.Login (Username => Username,
                                                   Secret => Secret);
   begin
      AMI.IO.Send (Socket, Command);
      Last_Action := Login;
   end Login;

   procedure Logoff (Socket : in Socket_Type) is
      Command : constant String := Protocol.Logoff;
   begin
      AMI.IO.Send (Socket, Command);
      Last_Action := Logoff;
   end Logoff;

   procedure Park (Socket           : in Socket_Type;
                   Channel          : in String;
                   Fallback_Channel : in String) is
      Command : constant String :=
        Protocol.Park (Channel          => Channel,
                       Fallback_Channel => Fallback_Channel);
   begin
      Ada.Text_IO.Put_Line ("Parking Call");
      Ada.Text_IO.Put_Line (Command);
      AMI.IO.Send (Socket, Command);
   end Park;

   procedure Ping (Socket : in Socket_Type) is
      Command : constant String := Protocol.Ping;
   begin
      AMI.IO.Send (Socket, Command);
      Last_Action := Ping;
   end Ping;

   procedure QueuePause (Socket     : in Socket_Type;
                         DeviceName : in String;
                         State      : in Protocol.Pause_States) is
      Command : constant String :=
        Protocol.QueuePause
         (DeviceName => DeviceName,
          State      => State);
   begin
      AMI.IO.Send (Socket, Command);
   end QueuePause;

   procedure QueueStatus (Socket   : in Socket_Type;
                          ActionID : in String := "") is
      Command : constant String := Protocol.QueueStatus (ActionID);
   begin
      AMI.IO.Send (Socket, Command);
      Last_Action := QueueStatus;
   end QueueStatus;

   procedure Redirect (Socket  : in Socket_Type;
                       Channel : in String;
                       Exten   : in String;
                       Context : in String := "LocalSets") is
      Command : constant String := Protocol.Redirect
        (Channel => Channel,
         Context => Context,
         Exten   => Exten,
         Priority => 1);
   begin
      AMI.IO.Send (Socket, Command);
   end Redirect;
end AMI.Action;
