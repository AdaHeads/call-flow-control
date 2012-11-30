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

   Reader    : Reader_Task;

   Callbacks : constant AMI.Event.Event_Callback_Table :=
     (PeerStatus         => My_Callbacks.Peer_Status'Access,
      Hangup             => My_Callbacks.Hangup'Access,
      Join               => My_Callbacks.Join'Access,
      Leave              => My_Callbacks.Leave'Access,
      Newchannel         => My_Callbacks.New_Channel'Access,
      Newstate           => My_Callbacks.New_State'Access,
      Dial               => My_Callbacks.Dial'Access,
      QueueCallerAbandon => My_Callbacks.Queue_Abandon'Access,
      others             => AMI.Event.Null_Callback'Access);

   procedure Authenticate;
   procedure Dispatch (Client : access AMI.Client.Client_Type;
                       Packet : in     AMI.Parser.Packet_Type);
   procedure Parser_Loop;
   procedure Connect;

   --  package My_Connection_Manager is new Connection_Management
   --    (Client   => Client_Access,
   --     Hostname => Config.Get (PBX_Host),
   --     Port     => Config.Get (PBX_Port));

   procedure Authenticate is
   begin
      Login (Client   => Client_Access,
             Username => Config.Get (PBX_User),
             Secret   => Config.Get (PBX_Secret));
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

   task body Reader_Task is
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
      Connect;
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
