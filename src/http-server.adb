with Ada.IO_Exceptions;

with Black.Request,
     Black.Response,
     GNAT.Sockets,
     GNAT.Sockets.Convenience;

with Configuration,
     Handlers.Route,
     System_Messages;

package body HTTP.Server is
   use GNAT.Sockets;
   Listener : Socket_Type;
   Selector : aliased Selector_Type;

   Current_State : Running_Status := Stopped;

   type Index is mod 2**3;

   task type Dispatchers is
      entry Handle_Request (Next_Connection : GNAT.Sockets.Socket_Type);
   end Dispatchers;

   Dispatcher : array (Index) of Dispatchers;
   Next       : Index := Index'First;

   task body Dispatchers is
      use Black.Response;
      Client : GNAT.Sockets.Socket_Type;
   begin
      loop
         select
            accept Handle_Request
              (Next_Connection : GNAT.Sockets.Socket_Type) do
               Client := Next_Connection;
            end Handle_Request;
         or
            terminate;
         end select;

         declare
            Request : constant Black.Request.Instance :=
              Black.Request.Parse_HTTP (Stream (Client));
         begin
            if Request.Want_Websocket then
               Instance'Output
                 (Stream (Client),
                  Black.Response.Switch_To_Websocket
                    (Key => Request.Websocket_Key));

            else

               Instance'Output
                 (Stream (Client), Handlers.Route.Callback (Request));
            end if;

            if not Request.Want_Websocket then
               Close_Socket (Socket => Client);
            end if;

         exception
            when Ada.IO_Exceptions.End_Error =>
               null;
            when Event : others =>
               System_Messages.Critical_Exception
                 (Message => "Oh noes!",
                  Event   => Event,
                  Context => "HTTP.Dispatcher");
               Close_Socket (Socket => Client);
         end;
      end loop;
   end Dispatchers;

   procedure Run is
      Context : constant String := Package_Name & ".Run";
   begin
      Listener := Convenience.Make_Server (Port => Configuration.HTTP_Port);
      Create_Selector (Selector);

      Current_State := Running;
      loop
         declare
            Connection : Socket_Type;
            Client     : Sock_Addr_Type;
            Status     : Selector_Status;
         begin

            Accept_Socket (Server   => Listener,
                           Socket   => Connection,
                           Address  => Client,
                           Timeout  => 0.2,
                           Selector => Selector'Access,
                           Status   => Status);

            if Status = Completed then
               Next := Index'Succ (Next);

               Dispatcher (Next).Handle_Request (Connection);
            elsif Status = Aborted then
               exit;
            end if;
         end;
      end loop;
      System_Messages.Debug ("Stopped.", Context);
   end Run;

   function Status return Running_Status is
   begin
      return Current_State;
   end Status;

   procedure Stop is
      Context : constant String := Package_Name & ".Stop";
   begin
      System_Messages.Debug ("Stopping.", Context);
      Shutdown_Socket (Listener);
      Abort_Selector (Selector);
      Current_State := Stopped;
   end Stop;

end HTTP.Server;
