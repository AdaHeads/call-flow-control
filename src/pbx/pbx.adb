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

with AMI.Action;
with AMI.Event;
with AMI.Observers;
with AMI.Response;
with AMI.Parser;

with My_Configuration;
with System_Messages;

--  TODO: Cover all branches on status.
package body PBX is
   use Ada.Strings.Unbounded;
   use AMI.Action;
   use AMI.Parser;
   use System_Messages;
   use My_Configuration;

   function Get_Line return String is
   begin
      return Client.Get_Line;
   end Get_Line;

   task type Reader_Task is
      entry Start;
   end Reader_Task;
   --  Continous reader loop that is reponsible for reading and dispatching
   --  packets. Does not die unless the stop primitive is called.

   procedure Authenticate;
   --  Wraps the entire authentication mechanism and provides a neat callback
   --  for the On_Connect event in the AMI.Client.

   procedure Dispatch (Client : access AMI.Client.Client_Type;
                       Packet : in     AMI.Parser.Packet_Type);
   --  Dispatches to the appropriate handlers (response or event)

   procedure Connect;
   --  Wraps the connection and wait mechanism and provides a neat callback
   --  for the On_Disconnect event in the AMI.Client.

   Reader    : Reader_Task;

   procedure Authenticate is
   begin
      Login (Client   => Client_Access,
             Username => Config.Get (PBX_User),
             Secret   => Config.Get (PBX_Secret));
      --  TODO: Replace this with a real wait for reply, and turn the
      --  Action into a synchronous call
      delay 0.2;

      --  Reload the state;
      AMI.Action.Core_Show_Channels (Client => Client_Access);
      AMI.Action.SIP_Peers (Client => Client_Access);

   end Authenticate;

   procedure Connect is
   begin

      --  TODO: Add cooldown to prevent hammering the Asterisk server.
      if PBX_Status /= Shutdown then
         PBX_Status := Connecting;
         System_Messages.Notify
           (Information, "PBX.Connect: Connecting");
         Client.Connect (Config.Get (PBX_Host), Config.Get (PBX_Port));

         if Client.Connected then
            PBX_Status := Running;
         end if;
      end if;

   end Connect;

   procedure Dispatch (Client : access AMI.Client.Client_Type;
                       Packet : in     AMI.Parser.Packet_Type) is
   begin
      --  System_Messages.Notify (Debug, Image (Packet => Packet));
      if Packet.Header.Key = AMI.Parser.Response then
         AMI.Response.Notify (Client => Client,
                              Packet => Packet);
      elsif Packet.Header.Key = AMI.Parser.Event then
         AMI.Observers.Notify (Event  => AMI.Event.Event_Type'Value
                               (To_String (Packet.Header.Value)),
                               Packet => Packet);
      else
         System_Messages.Notify (Error, "PBX.Dispatch: Bad header " &
                                   Packet.Header.Key'Img);
      end if;
   exception
      when E : others =>
         System_Messages.Notify
           (Error, "PBX.Dispatch: Failed to dispatch " &
              To_String (Packet.Header.Value) & " failed, on packet " &
              Ada.Exceptions.Exception_Information (E) &
              "" &
              "------------ Packet dump start ------" &
              Image (Packet) &
              "------------ Packet dump end ------" &
              "");
   end Dispatch;

   task body Reader_Task is
      procedure Parser_Loop;
      --  Wraps the continous Read_Packet, Dispatch calls and catches
      --  exceptions.

      procedure Parser_Loop is
      begin
         loop
            exit when PBX_Status = Shutdown;
            Client.Wait_For_Connection;
            Dispatch (Client_Access, Read_Packet (Get_Line'Access));
         end loop;
      exception
         when E : others =>
            if PBX_Status /= Shutdown then
               System_Messages.Notify
                 (Error, "PBX.Reader_Loop: Socket disconnected! ");
               System_Messages.Notify
                 (Error, Ada.Exceptions.Exception_Information (E));
            end if;
      end Parser_Loop;

   begin
      accept Start;
      loop
         exit when PBX_Status = Shutdown;
         Parser_Loop;
      end loop;
      System_Messages.Notify
        (Information, "PBX.Reader_Task: Shutting down.");
   exception
      when E : others =>
         System_Messages.Notify
           (Critical, "PBX.Reader_Task: Unexpected exception: ");
         System_Messages.Notify
           (Critical, Ada.Exceptions.Exception_Information (E));
   end Reader_Task;

   procedure Start is
   begin
      Client := AMI.Client.Create (On_Connect    => Authenticate'Access,
                                   On_Disconnect => Connect'Access);
      Connect; --  Initial connect.
      Reader.Start;
      System_Messages.Notify (Debug, "PBX Subsystem started");
   end Start;

   function Status return PBX_Status_Type is
   begin
      return PBX_Status;
   end Status;

   procedure Stop is
   begin
      System_Messages.Notify (Debug, "PBX.Shutdown");
      PBX_Status := Shutting_Down;
      Client.Disconnect;
      PBX_Status := Shutdown;
   end Stop;
end PBX;
