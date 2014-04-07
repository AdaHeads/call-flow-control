with
  GNAT.Sockets;

package HTTP_Server is
   procedure Listen (On_Port : in     GNAT.Sockets.Port_Type);
   procedure Stop;
end HTTP_Server;
