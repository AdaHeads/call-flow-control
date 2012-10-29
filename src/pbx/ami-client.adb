with Ada.Calendar;
with Ada.Text_IO;
with Ada.Exceptions;

with AWS.Net;
with AWS.Net.Std;
with AWS.Net.Buffered;

with AMI.Response;

with System_Messages;

package body AMI.Client is
   use System_Messages;
   
   function Is_Connected (Client  : access Client_Type) return Boolean is
      Socket_Event : AWS.Net.Event_Set;       
   begin
      Socket_Event := AWS.Net.Check
	(Socket  => Client.Socket,
	 Events  => (AWS.Net.Input => True, AWS.Net.Output => True));
      
      if Socket_Event (AWS.Net.Input) and then 
	Socket_Event (AWS.Net.Output) then
	 return True;
      else
	 return False;
      end if ;
   end Is_Connected;
   --  Determines if a server is connected via a socket.
   
   procedure Login
     (Client   : access Client_Type;
      Username : in     String;
      Secret   : in     String;
      Callback : in     AMI.Callback.Callback_Type 
	:= AMI.Callback.Login_Callback'Access) is
      Action_ID : constant Action_ID_Type := 
	Protocol_Strings.Next_Action_ID;
   begin
      AMI.Response.Subscribe (Action_ID,Callback);
      System_Messages.Notify (Debug, "AMI.Client.Login: Subscribed for " &
				"a reply with ActionID" & Action_ID'Img);
      Send (Client => Client, 
	    Item   => Protocol_Strings.Login 
	      (Username  => Username,
	       Secret    => Secret,
	       Action_ID => Action_ID));
   end Login;
   
   procedure Ping (Client   : access Client_Type;
		   Callback : in     AMI.Callback.Callback_Type 
		     := AMI.Callback.Ping_Callback'Access) is
      Action_ID : Action_ID_Type := 
	Protocol_Strings.Next_Action_ID;
   begin
      AMI.Response.Subscribe (Action_ID,Callback);
      System_Messages.Notify (Debug, "AMI.Client.Ping: Subscribed for " &
				"a reply with ActionID" & Action_ID'Img);
      Send (Client => Client, 
	    Item   => Protocol_Strings.Ping 
	      (Action_ID => Action_ID));
  end Ping;
   
  procedure Connect (Client   : access Client_Type;
		      Hostname : in   String;
		      Port     : in     Natural) is
      use Ada.Calendar;
      function Current_Time return Ada.Calendar.Time renames Clock;
      
      Timeout   : constant Ada.Calendar.Time := Current_Time + 3.0;
   begin
      AWS.Net.Buffered.Shutdown (Client.Socket);
      
      Client.Connected := False;
      Client.Authenticated := Unknown;
      
      System_Messages.Notify (Debug, "Connecting to " & 
				Hostname & ":" &
				Positive'Image(Port));
      AWS.Net.Std.Connect (Socket => Client.Socket,
			   Host   => Hostname,
			   Port   => Port,
			   Wait   => False);
      Wait_For_Connection :
      loop
	 Client.Connected := Is_Connected(Client);
	 exit Wait_For_Connection when Client.Connected 
	   or Current_Time > Timeout;
	 delay 0.05;
      end loop Wait_For_Connection;
      
      System_Messages.Notify (Debug, "Connected to " & 
				Hostname & ":" &
				Positive'Image(Port));
      
      --  The first line in the transmission is the greeting
      Client.Server_Greeting := 
	To_Unbounded_String (AWS.Net.Buffered.Get_Line 
			       (Socket => Client.Socket));
      System_Messages.Notify (Debug, "Connect: Server greeted me with:" & 
				To_String (Client.Server_Greeting));
      
   exception
      when Error: others =>
	 Client.Connected := False;
	 Client.Authenticated := Unknown;
	 raise CONNECT_FAILED with Ada.Exceptions.Exception_Message(Error);
   end Connect;
   
   procedure Bridge (Client   : access Client_Type;
		     ChannelA : in     String;
                     ChannelB : in     String;
		     Callback : in     AMI.Callback.Callback_Type 
		     := AMI.Callback.Null_Callback'Access) is
      Action_ID : Action_ID_Type := 
	Protocol_Strings.Next_Action_ID;
   begin
      AMI.Response.Subscribe (Action_ID,Callback);
      Send ( Client => Client,
	     Item   => Protocol_Strings.Bridge (Channel1 => ChannelA, 
						Channel2 => ChannelB,
						Action_ID => Action_ID));
   end Bridge;
   
   procedure Send (Client : access Client_Type; 
		   Item   : in     String) is
   begin
      AWS.Net.Buffered.Put (Client.Socket, Item);
      AWS.Net.Buffered.Flush (Client.Socket);
      System_Messages.Notify (Debug, "Send: Sent " & Item);
   exception
      when Error: others =>
	 Ada.Text_IO.Put ("Send: Unexpected exception: ");
	 Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information(Error));
	 raise AMI_SOCKET_NOT_CONNECTED;
   end Send;
   
   
end AMI.Client;   
