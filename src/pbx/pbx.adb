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
with PBX.Event_Stream;
with ESL.Trace,
     ESL.Packet_Content_Type,
     ESL.Packet,
     ESL.Packet.Buffer;

with Configuration;
with System_Messages;

with Model.Call.Observers;
with Model.Peer.List.Observers;

with Util.Process_Control;
with ESL.Parsing_Utilities;

--  TODO: Cover all branches on status.
package body PBX is
   use Ada.Strings.Unbounded;
   use Ada.Calendar;
   use System_Messages;
   package Config renames Configuration;

   task type Connect_Task is
      entry Start;
   end Connect_Task;
   --  The sole purpose of the connect task is to ensure that we can
   --  return to the main context, and don't get caught in an
   --  infinite reconnect loop.

   task type Dispatcher_Tasks
     (Buffer : access ESL.Packet.Buffer.Synchronized_Buffer) is
      entry Start;
   end Dispatcher_Tasks;

   task type Reader_Tasks
     (Buffer : access ESL.Packet.Buffer.Synchronized_Buffer) is
      entry Start;
   end Reader_Tasks;

   procedure Handle_Authentication
     (Client   : in out ESL.Basic_Client.Instance;
      Password : in String);

   Packet_Buffer : aliased ESL.Packet.Buffer.Synchronized_Buffer;

   Connection_Maintainer : access Connect_Task;
   Dispatcher_Task       : access Dispatcher_Tasks;
   Reader_Task           : access Reader_Tasks;

   task body Dispatcher_Tasks is

      Context : constant String := Package_Name & ".Dispatcher_Tasks";

      Packet : ESL.Packet.Instance;
   begin
      select
         accept Start;
      or
         terminate;
      end select;

      System_Messages.Information (Message => "Starting.",
                                   Context => Context);

      loop
         exit when Shutdown;

         declare
         begin
            select
               Buffer.Pop (Packet);
            or
               delay 0.1;
            end select;

            if Packet.Is_Event then
               PBX.Event_Stream.Observer_Map.Notify_Observers
                 (Packet => Packet);
               Packet := ESL.Packet.Empty_Packet;
            elsif Packet.Is_Response then
               null; --  Ignore reponses.
            end if;
         exception
            when Event : others =>
               System_Messages.Critical_Exception
                 (Message => "Oh noes!",
                  Event   => Event,
                  Context => Context);
         end;
      end loop;

      System_Messages.Information (Message => "Shutting down",
                                   Context => Context);
   end Dispatcher_Tasks;

   task body Reader_Tasks is
      use ESL.Parsing_Utilities;

      Context : constant String := Package_Name & ".Reader_Tasks";

      Packet : ESL.Packet.Instance;
   begin
      select
         accept Start;
      or
         terminate;
      end select;

      System_Messages.Information (Message => "Starting.",
                                   Context => Context);
      loop
         exit when Shutdown;
         Packet := ESL.Parsing_Utilities.Read_Packet (Event_Client.Stream);

         if Packet.Is_Event then
            Buffer.Push (Packet);

         elsif Packet.Is_Response then
            null; --  Ignore reponses.
         end if;
      end loop;

      System_Messages.Information (Message => "Shutting down",
                                   Context => Context);
   exception
      when Event : Disconnected =>
         System_Messages.Critical_Exception (Message => "",
                                             Event   => Event,
                                             Context => Context);
         if not PBX.Shutdown then
            Event_Client.Signal_Disconnect;
            Client.Signal_Disconnect;
            Connection_Maintainer := new Connect_Task;
            Connection_Maintainer.Start;

         end if;

   end Reader_Tasks;

   ---------------
   --  Connect  --
   ---------------

   procedure Connect is
      use ESL.Basic_Client;

      Next_Reconnect : Ada.Calendar.Time := Clock;

   begin
      System_Messages.Information
              (Message => "Event client state " &Event_Client.State'Img,
               Context => "PBX.Connect");

      while Event_Client.State /= Connected loop
         exit when Shutdown;
         delay until Next_Reconnect;

         Next_Reconnect := Clock + 2.0;

         if not Shutdown then
            System_Messages.Information
              (Message => "Connecting event client to " &
                 Config.PBX_Host & ":" &
                 Config.PBX_Port'Img,
               Context => "PBX.Connect");
            Event_Client.Connect (Hostname => Config.PBX_Host,
                            Port     => Config.PBX_Port);
         end if;
      end loop;

      if Event_Client.State = Connected then
         Handle_Authentication (Client   => Event_Client,
                                Password => Config.PBX_Password);

         System_Messages.Debug (Message => "Subscribing to all for events",
                                Context => "PBX.Connect");

         Event_Client.Unmute_Event (Format => Plain,
                                    Event => "all");

         Reader_Task     := new Reader_Tasks (Packet_Buffer'Access);
         Reader_Task.Start;

         Next_Reconnect := Clock; --  Reset the clock to enable the other
                                  --  clients to be able to try to connect
                                  --  immidiately.
      end if;

      if not Shutdown then
         while Client.State /= Connected loop
            exit when Shutdown;
            delay until Next_Reconnect;

            Next_Reconnect := Clock + 2.0;

            if not Shutdown then
               System_Messages.Information
                 (Message => "Connecting to " & Config.PBX_Host & ":" &
                    Config.PBX_Port'Img,
                  Context => "PBX.Connect");
               Client.Connect (Hostname => Config.PBX_Host,
                               Port     => Config.PBX_Port);
            end if;
         end loop;
      end if;

      if Client.State = Connected then
         System_Messages.Debug (Message => "Subscribing to all for events",
                                Context => "PBX.Connect");

         Handle_Authentication (Client   => Client,
                                Password => Config.PBX_Password);
      end if;

   end Connect;

   --------------------
   --  Connect_Task  --
   --------------------

   task body Connect_Task is
   begin
      accept Start;

      Connect; --  Initial connect.
      System_Messages.Information (Message => "PBX subsystem task started",
                                   Context => "PBX.Start");
      PBX.Action.Update_Call_List;
      PBX.Action.Update_SIP_Peer_List;

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

   procedure Handle_Authentication
     (Client   : in out ESL.Basic_Client.Instance;
      Password : in String)
   is
      use ESL.Packet_Content_Type;

      Current_Packet : ESL.Packet.Instance := ESL.Packet.Empty_Packet;

   begin
      --  Skip until the auth request.
      while Current_Packet.Content_Type /= Auth_Request loop
         Current_Packet := ESL.Parsing_Utilities.Read_Packet (Client.Stream);
      end loop;

      Client.Authenticate (Password => Password);

      System_Messages.Information
        (Message => "Authentication success.",
         Context => "PBX.Authenticate");

   exception
      when ESL.Basic_Client.Authentication_Failure =>
         System_Messages.Error
           (Message => "Authentication failure!",
            Context => "PBX.Authenticate");
      when ESL.Parsing_Utilities.Disconnected =>
         if Shutdown then
            null;
         else
            raise;
         end if;

         --  Ask the whole server to shutdown.
   end Handle_Authentication;

   procedure Start is
      use Config;
   begin
      case Config.PBX_Loglevel is
         when Critical =>
            ESL.Trace.Mute (Trace => ESL.Trace.Error);
            ESL.Trace.Mute (Trace => ESL.Trace.Warning);
            ESL.Trace.Mute (Trace => ESL.Trace.Debug);
            ESL.Trace.Mute (Trace => ESL.Trace.Information);
         when Error =>
            ESL.Trace.Mute (Trace => ESL.Trace.Debug);
            ESL.Trace.Mute (Trace => ESL.Trace.Warning);
            ESL.Trace.Mute (Trace => ESL.Trace.Information);
         when Warning =>
            ESL.Trace.Mute (Trace => ESL.Trace.Debug);
            ESL.Trace.Mute (Trace => ESL.Trace.Information);
         when Information =>
            ESL.Trace.Mute (Trace => ESL.Trace.Debug);
         when Debug | Fixme =>
            ESL.Trace.Unmute (Trace => ESL.Trace.Every);
      end case;

      System_Messages.Information (Message => "Registering observers.",
                                   Context => "PBX.Start");

      --  Register the appropriate observers.
      Model.Call.Observers.Register_Observers;
      Model.Peer.List.Observers.Register_Observers;

      Connection_Maintainer := new Connect_Task;
      Connection_Maintainer.Start;

      Dispatcher_Task := new Dispatcher_Tasks (Packet_Buffer'Access);
      Dispatcher_Task.Start;

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
      Shutdown := True;

      Model.Call.Observers.Unregister_Observers;
      Model.Peer.List.Observers.Unregister_Observers;
      System_Messages.Information
        (Message => "PBX subsystem task shutting down.",
         Context => "PBX.Stop");


      Client.Disconnect;
      Event_Client.Disconnect;
      System_Messages.Information
        (Message => "PBX subsystem task shutdown complete.",
         Context => "PBX.Stop");

   end Stop;
end PBX;
