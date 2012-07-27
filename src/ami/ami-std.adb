with Ada.Strings.Unbounded;
with Ada.Text_IO;
with AMI.Event;
with AWS.Net.Std;
with AWS.Net.Buffered;
with Yolk.Log;
with AMI.Action;
with Routines;
package body AMI.Std is
   Action_Socket : AWS.Net.Std.Socket_Type;
   Event_Socket : AWS.Net.Std.Socket_Type;
   --  it has package scope because, we need it in the Disconnect procedure.

   task AMI_Service is
      entry Start (Username : in String;
                   Secret   : in String);
   end AMI_Service;

   task body AMI_Service is
      use Ada.Strings.Unbounded;
      Username_Unbounded : Unbounded_String;
      Secret_Unbounded   : Unbounded_String;
   begin
      accept Start (Username : in String;
                    Secret   : in String) do
         Username_Unbounded := To_Unbounded_String (Username);
         Secret_Unbounded   := To_Unbounded_String (Secret);
      end Start;

      AMI.Event.Start (Event_Socket, To_String (Username_Unbounded),
                             To_String (Secret_Unbounded));

   exception
      when others =>
         Ada.Text_IO.Put_Line ("Exception in AMI.adb");
   end AMI_Service;

   procedure Connect (Server_Host : in String := "Asterisk1";
                      Server_Port : in Positive := 5038;
                      Username    : in String := "filtertest";
                      Secret      : in String := "filtertest") is
   begin
      --  Setting up event Socket.
      AWS.Net.Std.Connect (Socket => Event_Socket,
                           Host   => Server_Host,
                           Port   => Server_Port);
      Yolk.Log.Trace (Yolk.Log.Info,
                      "AMI Event socket connected - Host: "
                      & Server_Host & " Port: " & Server_Port'Img);
      AMI_Service.Start (Username, Secret);

      --  Setting up Action Socket.
      AWS.Net.Std.Connect (Socket => Action_Socket,
                           Host   => Server_Host,
                           Port   => Server_Port);
      Yolk.Log.Trace (Yolk.Log.Info,
                      "AMI Action socket connected - Host: "
                      & Server_Host & " Port: " & Server_Port'Img);

      AMI.Action.Initialize (Action_Socket,
                             "action",
                             "reaction");
      Yolk.Log.Trace (Yolk.Log.Debug, "AMI Action Initialized.");

      Routines.StartUpSequence;
   end Connect;

   procedure Disconnect is
      use Ada.Text_IO;
   begin
      AWS.Net.Buffered.Shutdown (Event_Socket);
      AWS.Net.Buffered.Shutdown (Action_Socket);
   end Disconnect;
end AMI.Std;
