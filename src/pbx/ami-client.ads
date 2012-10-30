package AMI.Client is
   
   procedure Connect (Client   : access Client_Type;
		      Hostname : in   String;
		      Port     : in     Natural); 
   
   procedure Disconnect (Client : access Client_Type); 
   
   procedure Send (Client : access Client_Type; 
		   Item   : in     String);
   -- Send an abitrary string
   
   
end AMI.Client;
