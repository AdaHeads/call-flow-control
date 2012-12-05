with Ada.Strings.Unbounded;
with Ada.Exceptions;

with AMI.Action;
with AMI.Response;
with AMI.Parser;
with AMI.Event;

with My_Configuration;
with System_Messages;
with My_Callbacks;

--  TODO: Cover all branches on status.
package body PBX is
   use Ada.Strings.Unbounded;
   use AMI.Action;
   use AMI.Parser;
   use AMI.Event;
   use System_Messages;
   use My_Configuration;

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
   Callbacks : constant AMI.Event.Event_Callback_Table :=
                 (CoreShowChannel
                  => My_Callbacks.Core_Show_Channel'Access,
                  CoreShowChannelsComplete
                  => My_Callbacks.Core_Show_Channels_Complete'Access,
                  PeerStatus         => My_Callbacks.Peer_Status'Access,
                  Hangup             => My_Callbacks.Hangup'Access,
                  Join               => My_Callbacks.Join'Access,
                  Leave              => My_Callbacks.Leave'Access,
                  Newchannel         => My_Callbacks.New_Channel'Access,
                  Newstate           => My_Callbacks.New_State'Access,
                  Dial               => My_Callbacks.Dial'Access,
                  QueueCallerAbandon => My_Callbacks.Queue_Abandon'Access,
                  others             => My_Callbacks.Default_Callback'Access);
   --  Event dispatch table.

   procedure Authenticate is
   begin
      Login (Client   => Client_Access,
             Username => Config.Get (PBX_User),
             Secret   => Config.Get (PBX_Secret));
      --  TODO: Replace this with a real wait for reply, and turn the
      --  Action into a synchronous call
      delay 0.2;
         AMI.Action.Core_Show_Channels (Client => Client_Access);
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

         AMI.Event.Dispatch (Callback_Table => Callbacks,
                             Packet         => Packet);
      else
         System_Messages.Notify (Error, "PBX.Dispatch: Bad header " &
                                   Packet.Header.Key'Img);
      end if;
   exception
      when E : others =>
         System_Messages.Notify
           (Error, "PBX.Dispatch: Unexpected exception: " &
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
            Dispatch (Client_Access, Read_Packet (Client_Access));
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
