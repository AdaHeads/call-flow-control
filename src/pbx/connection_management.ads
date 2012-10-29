with AMI;

generic 
   Client   : access AMI.Client_Type;
   Hostname : String;
   Port     : Natural;
package Connection_Management is
   use AMI;
   
   Reconnection_Delay : constant Duration := 3.0;
      
   procedure Wait_For_Connection;
   --  Blocking call to enable tasks to wait for a connection.
   
   procedure Signal_Disconnect;
   
   procedure Start;
   
   procedure Shutdown;
private
   
   task Reconnection_Task is  
     entry Start;
   end Reconnection_Task;
   
   protected Connection is
      entry Wait_For_Connect;
      entry Wait_For_Disconnect;
      procedure Change_State (Connection : in Boolean);
   private
      Connected : Boolean := False;
   end Connection;
   
--   Connection : Connection_Type;
   
   Shutdown_Pending : Boolean := False;
   
end Connection_Management;
