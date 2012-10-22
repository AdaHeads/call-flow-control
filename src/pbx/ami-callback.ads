with AMI.Parser;

package AMI.Callback is
   use AMI.Parser;
   type Callback_Type is access procedure (Client : access Client_Type;
					   Packet : in     AMI.Parser.Packet_Type);
   
   procedure Login_Callback (Client : access Client_Type;
			     Packet : in     AMI.Parser.Packet_Type);
   
   procedure Null_Callback (Client : access Client_Type;
			    Packet : in     AMI.Parser.Packet_Type);
   
   procedure Ping_Callback (Client : access Client_Type;
			    Packet : in     Packet_Type);
   
   procedure Subscribe (AMI_Key_Type : in Action_ID_Type;
     		        Callback     : in AMI.Callback.Callback_Type);
   
end AMI.Callback;
