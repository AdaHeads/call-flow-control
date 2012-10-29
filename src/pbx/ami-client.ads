with Ada.Strings.Unbounded;

with AMI.Parser;
with AMI.Callback;
with AMI.Generic_Protocol_Strings;

package AMI.Client is
   use Ada.Strings.Unbounded;
   
   procedure Connect (Client   : access Client_Type;
		      Hostname : in   String;
		      Port     : in     Natural);
   
   procedure Login
     (Client   : access Client_Type;
      Username : in     String;
      Secret   : in     String;
      Callback : in     AMI.Callback.Callback_Type 
	:= AMI.Callback.Login_Callback'Access
     );
   
   procedure Ping (Client   : access Client_Type;
		   Callback : in     AMI.Callback.Callback_Type 
		     := AMI.Callback.Ping_Callback'Access);
   
private 
   package Protocol_Strings is 
      new AMI.Generic_Protocol_Strings (Asynchronous => True);
   use Protocol_Strings;
   
   procedure Send (Client : access Client_Type; 
		   Item   : in     String);
      
end AMI.Client;
