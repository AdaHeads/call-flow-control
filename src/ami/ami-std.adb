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

with Ada.Exceptions;
with AMI.Action;
with AMI.Event;
with AWS.Net.Std;
with AWS.Net.Buffered;
with My_Configuration;
with Yolk.Log;

package body AMI.Std is

   Action_Socket : AWS.Net.Std.Socket_Type;
   Event_Socket  : AWS.Net.Std.Socket_Type;
--     Timeout       : constant Duration := 1.0;
   Shutdown      : Boolean := False;

   task AMI_Action_Task is
      entry Start;
      --  TODO: Write comment
   end AMI_Action_Task;

   task AMI_Event_Task is
      entry Start;
      --  TODO: Write comment.
   end AMI_Event_Task;

   -----------------------
   --  AMI_Action_Task  --
   -----------------------

   task body AMI_Action_Task
   is
      use My_Configuration;
      use Yolk.Log;

      Reconnect_Delay : constant Duration := 1.0;
      Connected : Boolean := False;
   begin
      select
         accept Start do
            Trace (Info, "AMI_Action_Task.Start, first line");
            AWS.Net.Std.Connect (Socket => Action_Socket,
                                 Host   => Config.Get (PBX_Host),
                                 Port   => Config.Get (PBX_Port));
            Connected := True;
         end Start;
      or
         delay 3.0;
         Trace (Error, "DEBUGGING POINT!");
         raise Program_Error;
      end select;
      Trace (Info, "Dropped out...of schoooool");

      Reconnect :
      loop
         begin
            if not Connected then
               AWS.Net.Std.Connect (Socket => Action_Socket,
                                    Host   => Config.Get (PBX_Host),
                                    Port   => Config.Get (PBX_Port));
            else
               Trace (Info, "DEBUG, action is connected, and do not try.");
            end if;

            Trace (Info,
                   "AMI action socket connected - Host: "
                   & Config.Get (PBX_Host)
                   & " Port: " & Config.Get (PBX_Port));

            Trace (Info, "DEBUG, Calling Action Start");
            AMI.Action.Start (Socket   => Action_Socket,
                              Username => Config.Get (PBX_Action_User),
                              Secret   => Config.Get (PBX_Action_Secret));
            Trace (Debug, "AMI action returned out of start");

         exception
            when AWS.Net.Socket_Error =>
               Trace (Error, "AMI.Action lost connection");
            when Err : others =>
               Trace (Error,
                      "ami-std, AMI Action, " &
                        "ExceptionName: " &
                        Ada.Exceptions.Exception_Name (Err));
         end;
         Connected := False;
         if Shutdown then
            Trace (Info, "AMI action connection Closed");
            exit Reconnect;
         end if;

         delay Reconnect_Delay;
      end loop Reconnect;
   exception
      when Program_Error =>
         Trace (Error, "Program Error DEBUG");
--        when AWS.Net.Socket_Error =>
--           Trace (Error, "AWS.Net.Socket_Error => DEBUG");
   end AMI_Action_Task;

   ----------------------
   --  AMI_Event_Task  --
   ----------------------

   task body AMI_Event_Task
   is
      use Ada.Exceptions;
      use My_Configuration;
      use Yolk.Log;

      Reconnect_Delay : constant Duration := 1.0;
      Connected : Boolean := False;
   begin
      --  accept Start;
      --  socket coonect
      --  AWS.Net.Std.Connect (Socket => Event_Socket,
      --                       Host   => Config.Get (PBX_Host),
      --                       Port   => Config.Get (PBX_Port));
      select
         accept Start do
            Yolk.Log.Trace (Yolk.Log.Info, "AMI Evnet connecting to: " &
                              Config.Get (PBX_Host) & ":" &
                              Config.Get (PBX_Port));

            AWS.Net.Std.Connect (Socket => Event_Socket,
                                 Host   => Config.Get (PBX_Host),
                                 Port   => Config.Get (PBX_Port));
            Connected := True;
         end Start;
      or
         delay 3.0;
         raise Program_Error;
      end select;

      Reconnect :
      loop
         begin
            if not Connected then
               AWS.Net.Std.Connect (Socket => Event_Socket,
                                    Host   => Config.Get (PBX_Host),
                                    Port   => Config.Get (PBX_Port));
            end if;
            Trace (Info,
                   "AMI event socket connected - Host: "
                   & Config.Get (PBX_Host)
                   & " Port: " & Config.Get (PBX_Port));

            AMI.Event.Start (Channel  => Event_Socket,
                             Username => Config.Get (PBX_Event_User),
                             Secret   => Config.Get (PBX_Event_Secret));
            Connected := False;
         exception
            when Err : others =>
               Trace (Error,
                      "ami-std, AMI Event, " &
                        "ExceptionName: " &
                        Ada.Exceptions.Exception_Name (Err));
            Connected := False;
         end;

         if Shutdown then
            Trace (Info, "AMI socket connection closed.");
            exit Reconnect;
         else
            --  send message out to websocket about system failure.
            Trace (Error, "No connection to AMI.Event");
         end if;

         delay Reconnect_Delay;
      end loop Reconnect;
--     exception
--        when AWS.Net.Socket_Error =>
--           Trace (Error, "AWS.Net.Socket_Error => DEBUG");
--        when Err : others =>
--           Trace
--             (Debug,
--              "Exception in AMI-STD.AMI_Service:" &
--                Exception_Name (Err) & "|:|" & Exception_Message (Err));
   end AMI_Event_Task;

   ---------------
   --  Connect  --
   ---------------

   procedure Connect
   is
      use My_Configuration;
      use Yolk.Log;
   begin
      Trace (Info, "DEBUG, Calling AMI_Event_Task.Start, in std connect");

      AMI_Event_Task.Start;

      Trace (Info, "AMI event socket initialized.");

      AMI_Action_Task.Start;

      Trace (Info, "AMI action socket initialized.");

   exception
      when AWS.Net.Socket_Error =>
         Trace (Error,
                "The hostname specified in the configuration file, " &
                  "could not be reached." &
                  Config.Get (PBX_Host) & ":" & Config.Get (PBX_Port));
         raise;
      when Err : others =>
         --  TODO change this to the appropriate Exception Type
         Yolk.Log.Trace (Yolk.Log.Error,
                         "AMI-STD.connect failed with exception" &
                           Ada.Exceptions.Exception_Name (Err));
         raise AWS.Net.Socket_Error;
   end Connect;

   ------------------
   --  Disconnect  --
   ------------------

   procedure Disconnect
   is
   begin
      Shutdown := True;
      AWS.Net.Buffered.Shutdown (Event_Socket);
      AWS.Net.Buffered.Shutdown (Action_Socket);
   end Disconnect;

end AMI.Std;
