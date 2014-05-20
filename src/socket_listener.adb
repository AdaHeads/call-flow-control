with Ada.IO_Exceptions;
with Ada.Exceptions;

with GNAT.Sockets;
with Request, Response;

with Configuration,
     Handlers.Socket,
     System_Messages;

with Ada.Streams;

--  TODO: Add all clients to a list so we can disconnect them on server
--        shutdown.

package body Socket_Listener is
   use GNAT.Sockets;

   Server_Socket : Socket_Type;
   Selector : aliased Selector_Type;

   Current_State : Running_Status := Stopped;

   function Get_Line (Stream : access Ada.Streams.Root_Stream_Type'Class)
                      return String;

   type Index is mod 2**3;

   task type Dispatchers is
      entry Handle_Request (Next_Connection : GNAT.Sockets.Socket_Type);
   end Dispatchers;

   Dispatcher : array (Index) of Dispatchers;
   Next       : Index := Index'First;

   task body Dispatchers is
      Context : constant String := Package_Name & ".Dispatcher (Task)";

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
            Client_Request  : Request.Instance;
            Server_Response : Response.Instance;
         begin
            Client_Request :=
              Request.Create (Client => Client,
                              From   => Get_Line (Stream (Client)));

            System_Messages.Debug (Client_Request.Image, Context);

            Server_Response := Handlers.Socket.Handle_Request (Client_Request);

            String'Write
              (Stream (Client), Server_Response.To_JSON_String & ASCII.LF);

            if not Server_Response.Keep_Open then
               Shutdown_Socket (Client);
            end if;
         exception
            when Ada.IO_Exceptions.End_Error =>
               null;
            when Event : others =>
               System_Messages.Critical_Exception
                 (Message => "Oh noes!",
                  Event   => Event,
                  Context => "Socket.Dispatcher");
               String'Write (Stream (Client),
                             Response.Create
                               (Status      => Response.Internal_Error,
                                Description =>
                                Ada.Exceptions.Exception_Message (Event))
                             .To_JSON_String & ASCII.LF);
               Close_Socket (Socket => Client);
         end;
      end loop;
   end Dispatchers;

   function Get_Line (Stream : access Ada.Streams.Root_Stream_Type'Class)
                      return String is
      Char   : Character := ASCII.NUL;
      Buffer : String (1 .. 2048);
      Offset : Natural := 0;
   begin
      loop
         exit when Offset >= Buffer'Last or Char = ASCII.LF;
         Char := Character'Input (Stream);
         case Char is
            when ASCII.CR | ASCII.LF =>
               null;
            when others =>
               Offset := Offset + 1;
               Buffer (Offset) := Char;
         end case;
      end loop;

      return Buffer (Buffer'First .. Buffer'First + Offset - 1);
   end Get_Line;

   procedure Run is
      Context : constant String := Package_Name & ".Run";
   begin
      Create_Socket (Socket => Server_Socket);

      Set_Socket_Option (Socket => Server_Socket,
                         Option => (Name    => Reuse_Address,
                                    Enabled => True));

      Bind_Socket (Socket  => Server_Socket,
                   Address => (Family => Family_Inet,
                               Addr   => Loopback_Inet_Addr,
                               Port   => Configuration.Socket_Port));
      --  TODO: Turn port into config option.

      Listen_Socket (Socket => Server_Socket);

      Create_Selector (Selector);

      Current_State := Running;
      System_Messages.Debug ("Stated.", Context);
      loop
         declare
            Client_Connection : Socket_Type;
            Client            : Sock_Addr_Type;
            Status            : Selector_Status;
         begin

            Accept_Socket (Server   => Server_Socket,
                           Socket   => Client_Connection,
                           Address  => Client,
                           Timeout  => 0.2,
                           Selector => Selector'Access,
                           Status   => Status);

            if Status = Completed then
               Next := Index'Succ (Next);

               Dispatcher (Next).Handle_Request (Client_Connection);
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
      Shutdown_Socket (Server_Socket);
      Abort_Selector (Selector);
      Current_State := Stopped;
   end Stop;

end Socket_Listener;
