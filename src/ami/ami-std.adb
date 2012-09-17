-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                 AMI.Std                                   --
--                                                                           --
--                                  BODY                                     --
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

with Ada.Strings.Unbounded,
     Ada.Exceptions;

with AMI.Event,
     AMI.Action;

--  with Routines;

with Yolk.Log;

with AWS.Net.Std,
     AWS.Net.Buffered;

package body AMI.Std is
   function TS
     (US : in Ada.Strings.Unbounded.Unbounded_String)
      return String
      renames Ada.Strings.Unbounded.To_String;

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
      use Ada.Strings.Unbounded;
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
         AMI_Action_Task.Server_Host := To_Unbounded_String (Server_Host);
         AMI_Action_Task.Server_Port := Server_Port;

         AMI_Action_Task.Username := To_Unbounded_String (Username);
         AMI_Action_Task.Secret   := To_Unbounded_String (Secret);
      end Initialize;

      Reconnect :
      loop
         begin
            AWS.Net.Std.Connect (Socket => Action_Socket,
                                 Host   => To_String (Server_Host),
                                 Port   => Server_Port);

            Yolk.Log.Trace (Yolk.Log.Info,
                            "AMI Action socket connected - Host: "
                            & To_String (Server_Host)
                            & " Port: " & Server_Port'Img);

            AMI.Action.Start (Socket => Action_Socket,
                              Username => TS (Username),
                              Secret => TS (Secret));
            Yolk.Log.Trace
              (Yolk.Log.Debug, "AMI Action returned out of start");
         exception
            when Err : others =>
               Yolk.Log.Trace (Yolk.Log.Info,
                               "ami-std, AMI Action, " &
                                 "ExceptionName: " &
                                 Ada.Exceptions.Exception_Name (Err));
         end;
         if Shutdown then
            Yolk.Log.Trace (Yolk.Log.Info, "PBX Action connection Closed");
            exit Reconnect;
         end if;

         delay Reconnect_Delay;
      end loop Reconnect;
   end AMI_Action_Task;

   --  AMI-Event needs to have it's own Thread,
   --   because it constantly reads from the socket.
   task AMI_Event_Task is
      entry Initialize (Server_Host : in String;
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
         AMI_Event_Task.Server_Host := To_Unbounded_String (Server_Host);
         AMI_Event_Task.Server_Port := Server_Port;

         AMI_Event_Task.Username := To_Unbounded_String (Username);
         AMI_Event_Task.Secret   := To_Unbounded_String (Secret);
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
