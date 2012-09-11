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
   Shutdown : Boolean := False;
   --  it has package scope because, we need it in the Disconnect procedure.

   --  AMI-Event needs to have it's own Thread,
   --   because it constantly reads from the socket.
   task AMI_Service is
      entry Initialize (Server_Host : in String;
                        Server_Port : in Positive;
                        Username : in String;
                        Secret   : in String);
   end AMI_Service;

   task body AMI_Service is
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
            when others =>
               if Shutdown then
                  Yolk.Log.Trace (Yolk.Log.Info, "PBX connection closed.");
                  exit Reconnect;
               else
                  --  send message out to websocket about system failure.
                  Yolk.Log.Trace (Yolk.Log.Error,
                                  "No connection to PBX.");
               end if;

         end;
         delay Reconnect_Delay;

      end loop Reconnect;
   exception
      when Err : others =>
         Yolk.Log.Trace
           (Yolk.Log.Debug,
            "Exception in AMI-STD.AMI_Service:" &
            Exception_Name (Err) & "|:|" & Exception_Message (Err));
   end AMI_Service;

   procedure Connect (Server_Host     : in String   := "Asterisk100";
                      Server_Port     : in Positive := 5038;
                      Event_Username  : in String   := "filtertest";
                      Event_Secret    : in String   := "filtertest"
--                        Action_Username : in String   := "action";
--                        Action_Secret   : in String   := "reaction"
                     ) is

   begin
      --  Socket connecting

      --  Setting up event Socket.
      AMI_Service.Initialize (Server_Host, Server_Port,
                              Event_Username, Event_Secret);

      --  Setting up Action Socket.
--        AMI.Action.Action_Manager.Initialize (Server_Host => Server_Host,
--                                              Server_Port => Server_Port,
--                                              Username => Action_Username,
--                                              Secret   => Action_Secret);
      Yolk.Log.Trace (Yolk.Log.Debug, "AMI Action Initialized.");

--        Routines.StartUpSequence;
   end Connect;

   procedure Disconnect is
   begin
      Shutdown := True;
      AWS.Net.Buffered.Shutdown (Event_Socket);
   end Disconnect;
end AMI.Std;
