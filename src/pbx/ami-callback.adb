with System_Messages;
with Ada.Strings.Unbounded;

package body AMI.Callback is
   use System_Messages;
   
   
   Authentication_Accepted_String : constant String := "Authentication accepted";
   Authentication_Failed_String   : constant String := "Authentication failed";
   
   --  Callback_Table : constant Event_Callback_Routine_Table :=
   --    (--Ping                 => Ping_Callback'Access,
   --     --  Dial                 => Dial_Callback'Access,
   --     --  Hangup               => Hangup_Callback'Access,
   --     --  Join                 => Join_Callback'Access,
   --     --  Newchannel           => Newchannel_Callback'Access,
   --     --  PeerStatus           => PeerStatus_Callback'Access,
   --     others               => null);
   --  --  Table for looking up event handlers.
   
   
   procedure Null_Callback (Client : access Client_Type;
			    Packet : in     AMI.Parser.Packet_Type) is
   begin
      System_Messages.Notify (Debug, "Null callback");
   end Null_Callback;
   
   procedure Login_Callback (Client : access Client_Type;
			     Packet : in     AMI.Parser.Packet_Type) is
      use Ada.Strings.Unbounded;
      Value : Unbounded_String;
   begin
      if Client /= null and then Packet /= New_Packet Then
	 if Try_Get (Packet.Fields, Message, Value) then
	    if To_String(Value) = Authentication_Accepted_String then
	       Client.Authenticated := Authenticated;	
	       
	    elsif To_String(Value) = Authentication_Failed_String then
	       Client.Authenticated := Not_Authenticated;	
	       System_Messages.Notify (Error, "Login_Callback: " & 
					 Authentication_Failed_String) ;	    
	    else
	       Client.Authenticated := Unknown;
	       
	       System_Messages.Notify (Error, "Login_Callback: Bad Reply:" & 
					 To_String(Value));
	    end if;
	 end if;
      end if;
   end Login_Callback;
   
   procedure Subscribe (AMI_Key_Type : in Action_ID_Type;
     		        Callback      : in AMI.Callback.Callback_Type) is
   begin
      null;
   end Subscribe;
   
   procedure Ping_Callback (Client : access Client_Type;
			    Packet : in     Packet_Type) is
   begin
      if Client /= null and then Packet /= New_Packet Then
	 
	 System_Messages.Notify 
	   (Debug, "Hello there, Ping - you are looking swell this evening.");
      end if;
   end Ping_Callback;
      
end AMI.Callback;
