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

with AMI.Action;
with AMI.Event;
with AWS.Net.Std;
with AWS.Net.Buffered;
with Errors;
with My_Configuration;
with Yolk.Log;

package body AMI.Std is
   use My_Configuration;
   AMI_Event_Error : exception;

   Action_Socket          : AWS.Net.Std.Socket_Type;
   Event_Socket           : AWS.Net.Std.Socket_Type;

   Connect_Delay          : constant Duration := 0.5;
   Host_Port              : constant String := Config.Get (PBX_Host) &
                              ":" &
                              Config.Get (PBX_Port);
   Socket_Connect_Timeout : constant Duration := 2.0;
   Shutdown               : Boolean := False;
   Task_Start_Timeout     : constant Duration := 3.0;
   Timed_Out_Message      : constant String := "Connecting to "
                              & Host_Port
                              & " timed out";

   task AMI_Action_Task is
      entry Start;
      --  TODO: Write comment.
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
      use Errors;
      use Yolk.Log;

      Socket_Event : AWS.Net.Event_Set;
   begin
      --        select
      --           accept Start do
      --              select
      --                 delay Socket_Connect_Timeout;
      --                 raise AMI_Action_Error with Timed_Out_Message;
      --              then abort
      --                 AWS.Net.Std.Connect (Socket => Action_Socket,
      --                                      Host   => Config.Get (PBX_Host),
      --                                      Port   => Config.Get (PBX_Port));
      --                 Connected := True;
      --              end select;
      --           end Start;
      --        or
      --           delay Task_Start_Timeout;
      --     raise AMI_Action_Error with "Start entry not called within time";
      --        end select;
      accept Start;

      Socket_Connection :
      loop
         begin
            AWS.Net.Std.Connect (Socket => Action_Socket,
                                 Host   => Config.Get (PBX_Host),
                                 Port   => Config.Get (PBX_Port),
                                 Wait   => False);

            delay Connect_Delay;
            --  TODO: Make this delay a configuration parameter. We could be
            --  trying to connect to an exceptionally slow server, in which
            --  case being able to tweak this value is probably a good idea.

            Socket_Event := AWS.Net.Check
              (Socket  => Action_Socket,
               Events  => (AWS.Net.Input => True, AWS.Net.Output => True));

            if Socket_Event (AWS.Net.Input)
              and then Socket_Event (AWS.Net.Output)
            then
               Trace (Info, "Connection to "
                      & Host_Port
                      & " AMI Action socket succeeded.");

               AMI.Action.Start (Socket   => Action_Socket,
                                 Username => Config.Get (PBX_Action_User),
                                 Secret   => Config.Get (PBX_Action_Secret));
            else
               --  Socket not ready, for some reason or another. Lets do a
               --  precautionary shutdown and then try connecting again.
               AWS.Net.Buffered.Shutdown (Action_Socket);

               Error_Handler ("Connection to "
                              & Host_Port
                              & " AMI Action socket failed.");
            end if;

         exception
            when E : AWS.Net.Socket_Error =>
               if not Shutdown then
                  Error_Handler
                    (Event   => E,
                     Message =>
                       "AMI_Action_Task lost connection to AMI socket");
               end if;
            when E : others =>
               Error_Handler
                 (Event   => E,
                  Message => "Error in AMI_Action_Task. We might've lost the"
                  & " connection to the AMI socket. Precautionary socket"
                  & " shutdown under way and then we'll try connecting again");

               AWS.Net.Buffered.Shutdown (Action_Socket);
         end;

         if Shutdown then
            exit Socket_Connection;
         end if;
      end loop Socket_Connection;

      Trace (Info, "AMI_Action_Task exited its main loop."
             & " Task completion imminent.");
   end AMI_Action_Task;

   ----------------------
   --  AMI_Event_Task  --
   ----------------------

   task body AMI_Event_Task
   is
      use Yolk.Log;

      Connected : Boolean := False;
   begin
      select
         accept Start do
            select
               delay Socket_Connect_Timeout;
               raise AMI_Event_Error with Timed_Out_Message;
            then abort
               AWS.Net.Std.Connect (Socket => Event_Socket,
                                    Host   => Config.Get (PBX_Host),
                                    Port   => Config.Get (PBX_Port));
               Connected := True;
            end select;
         end Start;
      or
         delay Task_Start_Timeout;
         raise AMI_Event_Error with "Start entry not called within time";
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
            when AWS.Net.Socket_Error =>
               Connected := False;

         end;

         if Shutdown then
            Trace (Info, "AMI socket connection closed.");
            exit Reconnect;
         else
            --  send message out to websocket about system failure.
            Trace (Error, "No connection to AMI.Event");
         end if;

         delay Connect_Delay;
      end loop Reconnect;
   end AMI_Event_Task;

   ---------------
   --  Connect  --
   ---------------

   procedure Connect
   is
      use Yolk.Log;
   begin
      AMI_Event_Task.Start;
      AMI_Action_Task.Start;
   end Connect;

   ------------------
   --  Disconnect  --
   ------------------

   procedure Disconnect
   is
      use Yolk.Log;
   begin
      Shutdown := True;

      Trace (Info, "Shutting down connection to "
                   & Host_Port
                   & " AMI Action socket.");
      AWS.Net.Buffered.Shutdown (Action_Socket);

      Trace (Info, "Shutting down connection to "
                   & Host_Port
                   & " AMI Event socket.");
      AWS.Net.Buffered.Shutdown (Event_Socket);
   end Disconnect;

end AMI.Std;
