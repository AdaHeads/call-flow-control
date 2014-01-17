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

with Ada.Strings.Unbounded;
with Ada.Exceptions;
with Ada.Calendar;

with PBX.Action;
with ESL.Trace;

with Alice_Configuration;
with System_Messages;

with Model.Call.Observers;
with Model.Peer.List.Observers;

with Util.Process_Control;

--  TODO: Cover all branches on status.
package body PBX is
   use Ada.Strings.Unbounded;
   use Ada.Calendar;
   use System_Messages;
   use Alice_Configuration;

   task type Connect_Task is
      entry Start;
   end Connect_Task;
   --  The sole purpose of the connect task is to ensure that we can
   --  return to the main context, and don't get caught in an
   --  infinite reconnect loop.

   procedure Authenticate is
   begin
      Client.Authenticate (Password => Config.Get (PBX_Secret));

      System_Messages.Information
        (Message => "Authentication success.",
         Context => "PBX.Authenticate");
      PBX.Action.Update_Call_List;
      PBX.Action.Update_SIP_Peer_List;

   exception
      when ESL.Client.Tasking.Authentication_Failure =>
         System_Messages.Error
           (Message => "Authentication failure!",
            Context => "PBX.Authenticate");

         --  Ask the whole server to shutdown.
         Util.Process_Control.Stop;
   end Authenticate;

   ---------------
   --  Connect  --
   ---------------

   procedure Connect is
      Next_Reconnect : Ada.Calendar.Time := Clock;
   begin
      while not Client.Connected loop
         exit when Shutdown;
         delay until Next_Reconnect;

         Next_Reconnect := Clock + 2.0;

         if not Shutdown then
            System_Messages.Information
              (Message => "Connecting to " & Config.Get (PBX_Host) & ":" &
                 Config.Get (PBX_Port),
               Context => "PBX.Connect");
            Client.Connect (Hostname => Config.Get (PBX_Host),
                            Port     => Config.Get (PBX_Port));
         end if;
      end loop;

      if Client.Connected then
         System_Messages.Debug (Message => "Subscribing to all for events",
                                Context => "PBX.Connect");
         Client.Send (Item => "event plain all");

      end if;

   end Connect;

   --------------------
   --  Connect_Task  --
   --------------------

   task body Connect_Task is
   begin
      accept Start;
      --  ESL.Trace.Mute (ESL.Trace.Every);
      Connect; --  Initial connect.
      System_Messages.Information (Message => "PBX subsystem task started",
                                   Context => "PBX.Start");
   exception
      when E : others =>
         System_Messages.Critical
           (Message => "PBX subsystem failed to start!",
            Context => "PBX.Connect_Task");
         System_Messages.Critical
           (Message => Ada.Exceptions.Exception_Information (E),
            Context => "PBX.Connect_Task");
         --  Ask the whole server to shutdown.
         Util.Process_Control.Stop;
   end Connect_Task;

   procedure Start is
      use ESL.Trace;
      Loglevel        : constant PBX_Loglevels := PBX_Loglevel;
      Initial_Connect : Connect_Task;
   begin
      case Loglevel is
         when Critical =>
            ESL.Trace.Mute (Trace => Error);
            ESL.Trace.Mute (Trace => Warning);
            ESL.Trace.Mute (Trace => Debug);
            ESL.Trace.Mute (Trace => Information);
         when Error =>
            ESL.Trace.Mute (Trace => Debug);
            ESL.Trace.Mute (Trace => Warning);
            ESL.Trace.Mute (Trace => Information);
         when Warning =>
            ESL.Trace.Mute (Trace => Debug);
            ESL.Trace.Mute (Trace => Information);
         when Information =>
            ESL.Trace.Mute (Trace => Debug);
         when Debug | Fixme =>
            ESL.Trace.Unmute (Trace => Every);
      end case;

      Client := new ESL.Client.Tasking.Instance
        (On_Connect_Handler    => Authenticate'Access,
         On_Disconnect_Handler => ESL.Client.Ignore_Event);

      --  Register the appropriate observers.
      Model.Call.Observers.Register_Observers;
      Model.Peer.List.Observers.Register_Observers;

      Initial_Connect.Start;
   end Start;

   function Status return PBX_Status_Type is
   begin
      if Shutdown then
         return Shut_Down;
      end if;
      return Running;
   end Status;

   procedure Stop is
   begin
      Model.Call.Observers.Unregister_Observers;
      Model.Peer.List.Observers.Unregister_Observers;
      System_Messages.Information
        (Message => "PBX subsystem task shutting down.",
                                   Context => "PBX.Stop");

      Shutdown := True;

      Client.Shutdown;
      System_Messages.Information
        (Message => "PBX subsystem task shutdown complete.",
         Context => "PBX.Stop");

   end Stop;
end PBX;
