with AMI.Client;

generic
   Client     : access AMI.Client.Client_Type;
   Hostname   : String;
   Port       : Natural;
   --  TODO: Rip these out, and make a more generic connection manager
   --  Connect    : access procedure;
   --  On_Connect : access procedure;
package Connection_Management is
   use AMI;

   Reconnection_Delay : constant Duration := 3.0;

   procedure Wait_For_Connection;
   --  Blocking call to enable tasks to wait for a connection.

   procedure Signal_Disconnect;

   procedure Start;

   procedure Shutdown;
private

   type Connection_State_Type is (Connected, Not_Connected, Shutdown);

   task Reconnection_Task is
     entry Start;
   end Reconnection_Task;

   protected Connection is
      entry Wait_For_Connect;
      entry Wait_For_Disconnect;
      procedure Change_State (Connection : Connection_State_Type);
      function State return Connection_State_Type;
   private
      Connection_State : Connection_State_Type := Not_Connected;
   end Connection;

end Connection_Management;
