with
  GNAT.Sockets.Convenience;
with
  Connection_Queue,
  System_Messages,
  Util.Process_Control;

package body HTTP_Server is
   Context : constant String := "HTTP_Server";

   task Listener is
      entry Start (Port : in     GNAT.Sockets.Port_Type);
      entry Stop;
   end Listener;

   task body Listener is
      Server : GNAT.Sockets.Socket_Type;
   begin
      Connect_To_Port :
      declare
         Server_Port : GNAT.Sockets.Port_Type;
      begin
         accept Start (Port : in     GNAT.Sockets.Port_Type) do
            Server_Port := Port;
         end Start;

         Server := GNAT.Sockets.Convenience.Make_Server (Server_Port);
      end Connect_To_Port;

      loop
         declare
            use GNAT.Sockets;
            Connection : Socket_Type;
            Client     : Sock_Addr_Type;
         begin
            Accept_Socket (Server  => Server,
                           Socket  => Connection,
                           Address => Client);

            Connection_Queue.Object.Enqueue (Connection);

            select
               accept Stop;
               exit;
            else
               null;
            end select;
         end;
      end loop;
   exception
      when Event : others =>
         System_Messages.Critical_Exception
           (Message => "An exception terminated the listener task.",
            Event   => Event,
            Context => Context);
         Util.Process_Control.Stop;
   end Listener;

   procedure Listen (On_Port : in     GNAT.Sockets.Port_Type) is
   begin
      select
         Listener.Start (On_Port);
      or
         delay 1.0;
         System_Messages.Critical
           (Message => "Gave up waiting for the listener to accept a port.",
            Context => Context);
         raise Program_Error
           with "Gave up waiting for the listener to accept a port.";
      end select;
   end Listen;

   procedure Stop is
   begin
      select
         Listener.Stop;
      or
         delay 1.0;
         System_Messages.Critical
           (Message => "Gave up waiting for the listener to stop.  " &
                       "Aborting it.",
            Context => Context);
         abort Listener;
      end select;
   end Stop;
end HTTP_Server;
