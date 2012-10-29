with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;

with AWS.Net.Std;

package AMI is
   
   package Latin_1 renames Ada.Characters.Latin_1;
   
   
   type Autenticated_Type is (Unknown, No_Reply, 
  			      Authenticated, Not_Authenticated);
   type Client_Type is
      record
	 Connected       : Boolean := False;
	 Server_Greeting : Ada.Strings.Unbounded.Unbounded_String;
	 Authenticated   : Autenticated_Type := Unknown;
	 Socket          : AWS.Net.Std.Socket_Type;
      end record; 
   
   
   --  type Callback_Type is access procedure (Event_List : Event_List_Type.Map);
   --  type Callback_Routine_Table is array (Response_Type) of Callback_Type;

   --  CallBack_Routine : constant Callback_Routine_Table :=
   --    (others => null);

   Line_Termination_String : constant String := (1 => ASCII.CR, 2 => ASCII.LF);
   
   Packet_Termination_String : constant String := 
     Line_Termination_String & Line_Termination_String;
   
   Key_Value_Seperator : constant String := Latin_1.Colon & Latin_1.Space;
   
   AMI_SOCKET_NOT_CONNECTED     : exception;
   AMI_SOCKET_NOT_AUTHENTICATED : exception;
   NOT_IMPLEMENTED              : exception;
   CONNECT_FAILED               : exception;   
   
   type Action_ID_Type is mod 2**16;
   
   Null_Action_ID : constant Action_ID_Type := -1;
   
end AMI;
