with GNAT.Sockets;
with GNATCOLL.JSON;

package Handlers.Event_Socket is
   use GNAT.Sockets;

   Package_Name : constant String := "Handlers.Event_Socket";

   procedure Register_Client (Client : in Socket_Type);
   procedure Unregister_Client (Client : in Socket_Type);

   procedure Broadcast (Item : in GNATCOLL.JSON.JSON_Value);

end Handlers.Event_Socket;
