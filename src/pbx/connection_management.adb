with Ada.Exceptions;

with System_Messages; use System_Messages;

package body Connection_Management is
   
   procedure Start is
   begin
      Reconnection_Task.Start;
   end Start;
   
   procedure Shutdown is
   begin
      Connection.Change_State (Shutdown);
      Client.Disconnect;
   end Shutdown;
   
   procedure Wait_For_Connection is
   begin
      Connection.Wait_For_Connect;
   end Wait_For_Connection;
   
   procedure Signal_Disconnect is
   begin
      Connection.Change_State (Not_Connected);
   end Signal_Disconnect;
   
   task body Reconnection_Task is
      procedure Try_Connect is
      begin
	 if not Client.Connected and Connection.State /= Shutdown then
	    Client.Connect (Hostname, Port);
	 end if;
	 
      exception 
	 when E: others =>
	    System_Messages.Notify (Error, "Connection_Manager: Unexpected exception: ");
	    System_Messages.Notify (Error, Ada.Exceptions.Exception_Information(E));
      end Try_Connect;	 
      
   begin
      accept Start;
      loop
	 exit when Connection.State = Shutdown;
	 
	 Try_Connect;
	   
	 if Client.Connected then
	    Connection.Change_State (Connected);
	 end if;
	 delay Reconnection_Delay;
	 Connection.Wait_For_Disconnect;
      end loop;
      System_Messages.Notify (Information, "Connection_Manager: Normal shutdown complete");

   end Reconnection_Task;
   
   protected body Connection is
      entry Wait_For_Connect when Connection.State = Connected or 
	Connection.State = Shutdown is
      begin
	 null;
      end Wait_For_Connect;
      
      entry Wait_For_Disconnect when Connection.State = Not_Connected or 
	Connection.State = Shutdown is
      begin
	 null;
      end Wait_For_Disconnect;
      
      procedure Change_State (Connection : Connection_State_Type) is
      begin
	 Connection_State := Connection;
      end Change_State;
      
      function State return Connection_State_Type is
      begin
	 return Connection_State;
      end State;
   end Connection;
end Connection_Management;
