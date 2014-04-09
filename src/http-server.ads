package HTTP.Server is

   Package_Name : constant String := "HTTP.Server";

   type Running_Status is (Running, Stopped);

   procedure Run;
   procedure Stop;
   function Status return Running_Status;

end HTTP.Server;
