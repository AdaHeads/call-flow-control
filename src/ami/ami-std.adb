with Ada.Strings.Unbounded,
     Ada.Text_IO;

with AMI.Event,
     AMI.Action;

with Routines,
     Task_Controller;

with Yolk.Log;

with AWS.Net.Std,
     AWS.Net.Buffered;

package body AMI.Std is
   --     Action_Socket : AWS.Net.Std.Socket_Type;
   Event_Socket : AWS.Net.Std.Socket_Type;
   --  it has package scope because, we need it in the Disconnect procedure.

   task AMI_Service is
      entry Initialize (Server_Host : in String;
                        Server_Port : in Positive;
                        Username : in String;
                        Secret   : in String);
   end AMI_Service;

   task body AMI_Service is
      use Ada.Strings.Unbounded;
      use Task_Controller;
      Server_Host : Unbounded_String;
      Server_Port : Positive;

      Username : Unbounded_String;
      Secret   : Unbounded_String;

   begin
      accept Initialize (Server_Host : in String;
                         Server_Port : in Positive;
                         Username    : in String;
                         Secret      : in String) do
         AMI_Service.Server_Host := To_Unbounded_String (Server_Host);
         AMI_Service.Server_Port := Server_Port;

         AMI_Service.Username := To_Unbounded_String (Username);
         AMI_Service.Secret   := To_Unbounded_String (Secret);
      end Initialize;

      Reconnect :
      loop
         begin
            exit Reconnect when Task_State = Down;
            AWS.Net.Std.Connect (Socket => Event_Socket,
                                 Host   => To_String (Server_Host),
                                 Port   => Server_Port);
            Yolk.Log.Trace (Yolk.Log.Info,
                            "AMI Event socket connected - Host: "
                            & To_String (Server_Host)
                            & " Port: " & Server_Port'Img);

            AMI.Event.Start (Event_Socket, To_String (Username),
                             To_String (Secret));
         exception
            when others =>
               Yolk.Log.Trace (Yolk.Log.Info,
                               "The Event Socket have lost connection");
         end;
         delay 0.5;
      end loop Reconnect;
   exception
      when others =>
         Ada.Text_IO.Put_Line ("Exception in AMI-STD.adb");
   end AMI_Service;

   procedure Connect (Server_Host : in String := "Asterisk1";
                      Server_Port : in Positive := 5038;
                      Username    : in String := "filtertest";
                      Secret      : in String := "filtertest") is
      Action_Username : constant String := "action";
      Action_Secret : constant String := "reaction";

   begin
      --  Setting up event Socket.
      AMI_Service.Initialize (Server_Host, Server_Port, Username, Secret);

      --  Setting up Action Socket.
      AMI.Action.Action_Manager.Initialize (Server_Host => Server_Host,
                                            Server_Port => Server_Port,
                                            Username => Action_Username,
                                            Secret   => Action_Secret);
      Yolk.Log.Trace (Yolk.Log.Debug, "AMI Action Initialized.");

      Routines.StartUpSequence;
   end Connect;

   procedure Disconnect is
      use Ada.Text_IO;
   begin
      AWS.Net.Buffered.Shutdown (Event_Socket);
      --        AWS.Net.Buffered.Shutdown (Action_Socket);
   end Disconnect;
end AMI.Std;
