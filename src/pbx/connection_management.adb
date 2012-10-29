with Ada.Exceptions;

with System_Messages; use System_Messages;
with AMI.Client;

package body Connection_Management is
   
   procedure Start is
   begin
      Reconnection_Task.Start;
   end Start;
   
   procedure Shutdown is
   begin
      Shutdown_Pending := True;
   end Shutdown;
   
   procedure Wait_For_Connection is
   begin
      Connection.Wait_For_Connect;
   end Wait_For_Connection;
   
   procedure Signal_Disconnect is
   begin
      Connection.Change_State (False);
   end Signal_Disconnect;
   
   task body Reconnection_Task is
      use AMI.Client;
      
      procedure Try_Connect is
      begin
	 if not Client.Connected then
	    Connect(Client, Hostname, Port);
	 end if;
	 
      exception 
	 when E: others =>
	    System_Messages.Notify (Error, "Connection_Manager: Unexpected exception: ");
	    System_Messages.Notify (Error, Ada.Exceptions.Exception_Information(E));
      end Try_Connect;	 
      
   begin
      accept Start;
      loop
	 Connection.Wait_For_Disconnect;
	 
	 exit when Shutdown_Pending;
	 
	 Try_Connect;
	   
	 if Client.Connected then
	    Connection.Change_State (True);
	 end if;
	 
	 delay Reconnection_Delay;
	   
      end loop;
   end Reconnection_Task;
   
   protected body Connection is
      entry Wait_For_Connect when Connected is
      begin
	 System_Messages.Notify (Debug,"Released (on connect)");
      end Wait_For_Connect;
      
      entry Wait_For_Disconnect when not Connected is
      begin
	 System_Messages.Notify (Debug,"Released (on disconnect)");
      end Wait_For_Disconnect;
      
      procedure Change_State (Connection : in Boolean) is
      begin
	 Connected := Connection;
	 System_Messages.Notify (Debug,"Connection state changed to " & Connected'Img);
      end Change_State;
   end Connection;
end Connection_Management;
