with
  GNAT.Sockets;
with
  Black.Request;
with
  Connection_Queue,
  Namespaces,
  Handlers.Call,
  Handlers.Not_Found,
  Handlers.Users,
  System_Messages;

package body Handlers.API_Tasks is

   task body Processor is
      Context : constant String := "API_Tasks.Processor";

      Connection : GNAT.Sockets.Socket_Type;
   begin
      loop
         begin
            Connection_Queue.Object.Dequeue (Connection);

            declare
               use GNAT.Sockets;
               Stream  : Stream_Access renames
                           GNAT.Sockets.Stream (Connection);
               Request : constant Black.Request.Instance :=
                           Black.Request.Parse_HTTP (Stream);
            begin
               case Namespaces.Resource (Request) is
                  when Namespaces.Not_Found =>
                     Handlers.Not_Found.Handle (Stream  => Stream,
                                                Request => Request);
                  when Namespaces.Call =>
                     Handlers.Call.Handle (Stream  => Stream,
                                           Request => Request);
                  when Namespaces.Users =>
                     Handlers.Users.Handle (Stream  => Stream,
                                            Request => Request);
               end case;

               Close_Socket (Connection);
            end;
         exception
            when Event : others =>
               System_Messages.Critical_Exception
                 (Message => "Connection processing terminated.",
                  Event   => Event,
                  Context => Context);
         end;
      end loop;
   exception
      when Event : others =>
         System_Messages.Critical_Exception
           (Message => "Task terminated.",
            Event   => Event,
            Context => Context);
   end Processor;
end Handlers.API_Tasks;
