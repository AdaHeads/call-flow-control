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
--  with Ada.Exceptions;
with Ada.Calendar;

--  with PBX.Action;

--  with PBX.Event_Handlers;
--  pragma Unreferenced (PBX.Event_Handlers);
with PBX.Call.Event_Handlers;
pragma Unreferenced (PBX.Call.Event_Handlers);

with Alice_Configuration;
with System_Messages;

--  TODO: Cover all branches on status.
package body PBX is
   use Ada.Strings.Unbounded;
   use Ada.Calendar;
   use System_Messages;
   use Alice_Configuration;

   Next_Reconnect : Ada.Calendar.Time := Clock;

   task Connect_Task is
      entry Start;
   end Connect_Task;
   --  The sole purpose of the connect task is to ensure that we can
   --  return to the main context, and don't get caught in an
   --  infinite reconnect loop.

   procedure Authenticate is
   begin
      System_Messages.Notify
           (Information, "PBX.Authenticate: Authenticating");
      Client.Authenticate (Password => Config.Get (PBX_Secret));
   end Authenticate;

   ---------------
   --  Connect  --
   ---------------

   procedure Connect is
   begin
      while not Client.Connected loop
         exit when Shutdown;
         delay until Next_Reconnect;
         Next_Reconnect := Clock + 2.0;

         if not Shutdown then
            System_Messages.Notify
              (Information, "PBX.Connect: Connecting");
            Client.Connect (Hostname => Config.Get (PBX_Host),
                            Port     => Config.Get (PBX_Port));
         end if;
      end loop;

      if Client.Connected then
         System_Messages.Notify
           (Information, "PBX.Connect: subscribing for events");
         Client.Send (Item => "event plain all");
         System_Messages.Notify
           (Information, "PBX.Connect: subscribed for events");
      end if;

   end Connect;

   --------------------
   --  Connect_Task  --
   --------------------

   task body Connect_Task is
   begin
      accept Start;
      Connect; --  Initial connect.
   end Connect_Task;

   procedure Start is
   begin
      Connect_Task.Start;
      System_Messages.Notify (Information, "PBX Subsystem started");
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
      System_Messages.Notify (Debug, "PBX.Shutdown");

      Shutdown := True;

      Client.Shutdown;
      System_Messages.Notify (Debug, "PBX.Shutdown complete");

   end Stop;
end PBX;
