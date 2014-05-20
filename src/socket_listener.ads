
package Socket_Listener is

   Package_Name : constant String := "Socket_Listener";

   type Running_Status is (Running, Stopped);

   procedure Run;
   procedure Stop;
   function Status return Running_Status;

end Socket_Listener;
