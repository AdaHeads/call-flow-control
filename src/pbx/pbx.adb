with Ada.Strings.Unbounded;
with Ada.Exceptions;

with AWS.Net;

with AMI.Action;
with AMI.Response;
with AMI.Parser;
with AMI.Event;

with Connection_Management;
with Configuration;
with System_Messages;
with My_Callbacks;

package body PBX is
   use Ada.Strings.Unbounded;
   use AMI.Action;
   use AMI.Parser;
   use AMI.Event;
   use System_Messages;

   task type Reader_Task is
      entry Start;
   end Reader_Task;

   Reader    : Reader_Task;

   Callbacks : constant AMI.Event.Event_Callback_Table :=
     (PeerStatus         => My_Callbacks.Peer_Status'Access,
      Hangup             => My_Callbacks.Hangup'Access,
      Join               => My_Callbacks.Join'Access,
      Newchannel         => My_Callbacks.New_Channel'Access,
      Newstate           => My_Callbacks.New_State'Access,
      Dial               => My_Callbacks.Dial'Access,
      QueueCallerAbandon => My_Callbacks.Queue_Abandon'Access,
      others             => AMI.Event.Null_Callback'Access);

   procedure Authenticate;
   procedure Dispatch (Client : access Client_Type;
                       Packet : in     AMI.Parser.Packet_Type);
   procedure Parser_Loop;
   procedure Reconnect;

   package My_Connection_Manager is new Connection_Management
     (Client   => Client_Access,
      Hostname => Configuration.Hostname,
      Port     => Configuration.Port);

   procedure Authenticate is
   begin
      My_Connection_Manager.Wait_For_Connection;

      Login (Client   => Client_Access,
             Username => Configuration.Username,
             Secret   => Configuration.Secret);
   end Authenticate;

   procedure Dispatch (Client : access Client_Type;
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
           (Error, "PBX.Dispatch: Unexpected exception: ");
         System_Messages.Notify
           (Error, Ada.Exceptions.Exception_Information (E));
         System_Messages.Notify
           (Error, "");
         System_Messages.Notify
           (Error, "------------ Packet dump start ------");
         System_Messages.Notify
           (Error, Image (Packet));
         System_Messages.Notify
           (Error, "------------ Packet dump end ------");
         System_Messages.Notify
           (Error, "");
   end Dispatch;

   procedure Parser_Loop is
   begin
      loop
         exit when Shutdown;
         Dispatch (Client_Access, Read_Packet (Client_Access));
      end loop;
   exception
      when E : AWS.Net.Socket_Error =>
         if not Shutdown then
            System_Messages.Notify
              (Error, "PBX.Reader_Loop: Socket disconnected! ");
            System_Messages.Notify
              (Error, Ada.Exceptions.Exception_Information (E));
            Reconnect;
         end if;
   end Parser_Loop;

   procedure Reconnect is
   begin
      Client.Connected := False;
      if not Shutdown then
         System_Messages.Notify
           (Information, "PBX.Reader_Loop: Signalling disconnect: ");

         My_Connection_Manager.Signal_Disconnect;
         Authenticate;
      end if;
   end Reconnect;

   task body Reader_Task is
   begin
      accept Start;
      loop
         exit when Shutdown;
         Parser_Loop;
      end loop;
   exception
      when E : others =>
         System_Messages.Notify
           (Critical, "PBX.Reader_Task: Unexpected exception: ");
         System_Messages.Notify
           (Critical, Ada.Exceptions.Exception_Information (E));
   end Reader_Task;

   procedure Start is
   begin
      My_Connection_Manager.Start;

      --  Initial authentication
      Authenticate;
      Reader.Start;
   end Start;

   procedure Status is
   begin
      null;
   end Status;

   procedure Stop is
   begin
      Shutdown := True;
      My_Connection_Manager.Shutdown;
   end Stop;
end PBX;
