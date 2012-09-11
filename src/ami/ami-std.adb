with Ada.Strings.Unbounded,
     Ada.Exceptions;

with AMI.Event;
--       AMI.Action;

--  with Routines;

with Yolk.Log;

with AWS.Net.Std,
     AWS.Net.Buffered;

package body AMI.Std is
   Event_Socket : AWS.Net.Std.Socket_Type;
   Action_Socket : AWS.Net.Std.Socket_Type;
   Shutdown : Boolean := False;
   --  it has package scope because, we need it to shutdown the connection.

   task AMI_Action_Task is
      entry Initialize (Server_Host : in String;
                        Server_Port : in Positive;
                        Username : in String;
                        Secret   : in String);
   end AMI_Action_Task;

   task body AMI_Action_Task is
      Reconnect_Delay : constant Duration := 1.0;

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

      Reconnect:
      loop
         begin
            AWS.Net.Std.Connect (Socket => Action_Socket,
                                 Host   => To_String (Server_Host),
                                 Port   => Server_Port);
            Yolk.Log.Trace (Yolk.Log.Info,
                            "AMI Action socket connected - Host: "
                            & To_String (Server_Host)
                            & " Port: " & Server_Port'Img);

            --  TODO, Start the work here.
         exception
            when Err : others =>
               Yolk.Log.Trace (Yolk.Log.Info,
                               "ami-std, AMI Action, " &
                                 "ExceptionName: " &
                                 Ada.Exceptions.Exception_Name (Err));
         end
         if Shutdown then
            Yolk.Log.Trace (Yolk.Log.Info, "PBX Action connection Closed")
            exit Reconnect;
         end if;
      end loop Reconnect;
   end AMI_Action_Task;

   --  AMI-Event needs to have it's own Thread,
   --   because it constantly reads from the socket.
   task AMI_Event_Task is
      entry Initialize (Server_Host : in String;
                        Server_Port : in Positive;
                        Username : in String;
                        Secret   : in String);(Server_Host : in String;
                                               Server_Port : in Positive;
                                               Username : in String;
                                               Secret   : in String);
   end AMI_Event_Task;

   task body AMI_Event_Task is
      use Ada.Strings.Unbounded;
      use Ada.Exceptions;

      Reconnect_Delay : constant Duration := 1.0;

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
            when Err : others =>
               Yolk.Log.Trace (Yolk.Log.Info,
                               "ami-std, AMI Event, " &
                                 "ExceptionName: " &
                                 Ada.Exceptions.Exception_Name (Err));

         end;
         if Shutdown then
            Yolk.Log.Trace (Yolk.Log.Info, "PBX connection closed.");
            exit Reconnect;
         else
            --  send message out to websocket about system failure.
            Yolk.Log.Trace (Yolk.Log.Error,
                            "No connection to PBX.");
         end if;
         delay Reconnect_Delay;

      end loop Reconnect;
   exception
      when Err : others =>
         Yolk.Log.Trace
           (Yolk.Log.Debug,
            "Exception in AMI-STD.AMI_Service:" &
              Exception_Name (Err) & "|:|" & Exception_Message (Err));
   end AMI_Event_Task;

   procedure Connect (Server_Host     : in String   := "Asterisk1";
                      Server_Port     : in Positive := 5038;
                      Event_Username  : in String   := "filtertest";
                      Event_Secret    : in String   := "filtertest";
                      Action_Username : in String   := "action";
                      Action_Secret   : in String   := "reaction"
                     ) is

   begin
      --  Socket connecting

      --  Setting up event Socket.
      AMI_Event_Task.Initialize (Server_Host, Server_Port,
                              Event_Username, Event_Secret);
      Yolk.Log.Trace (Yolk.Log.Debug, "AMI Event Initialized.");

      AMI_Action_Task.Initialize (Server_Host, Server_Port,
                              Action_Username, Action_Secret);
      Yolk.Log.Trace (Yolk.Log.Debug, "AMI Action Initialized.");

      --        Routines.StartUpSequence;
   end Connect;

   procedure Disconnect is
   begin
      Shutdown := True;
      AWS.Net.Buffered.Shutdown (Event_Socket);
   end Disconnect;
end AMI.Std;
