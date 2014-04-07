with
  GNAT.Sockets;
with
  Black.Request,
  Black.Response;
with
  Connection_Queue,
  System_Messages;

package body API_Tasks is

   task body Processor is
      Context : constant String := "API_Tasks.Processor";

      Connection : GNAT.Sockets.Socket_Type;
   begin
      loop
         begin
            Connection_Queue.Object.Dequeue (Connection);

            declare
               use GNAT.Sockets;
               Request : constant Black.Request.Instance :=
                           Black.Request.Parse_HTTP (Stream (Connection));
               use Black.Response;
            begin
               if Request.Resource = "/redirect" then
                  pragma Warnings (Off); --  Workaround for GNAT-4.6.
                  Instance'Output
                    (Stream (Connection),
                     Redirect (Target    => "http://www.jacob-sparre.dk/",
                               Permanent => False));
                  pragma Warnings (On); --  Workaround for GNAT-4.6.
               elsif Request.Resource = "/" then
                  pragma Warnings (Off); --  Workaround for GNAT-4.6.
                  Instance'Output
                    (Stream (Connection),
                     OK (Data => "You've visited the single threaded Black " &
                           "example server."));
                  pragma Warnings (On); --  Workaround for GNAT-4.6.
               else
                  pragma Warnings (Off); --  Workaround for GNAT-4.6.
                  Instance'Output (Stream (Connection),
                                   Not_Found (Resource => Request.Resource));
                  pragma Warnings (On); --  Workaround for GNAT-4.6.
               end if;

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
end API_Tasks;
