with Ada.Characters.Latin_1;
package AMI is

   package Latin_1 renames Ada.Characters.Latin_1;

   Line_Termination_String : constant String := (1 => ASCII.CR, 2 => ASCII.LF);

   Packet_Termination_String : constant String :=
     Line_Termination_String & Line_Termination_String;

   Key_Value_Seperator : constant String := Latin_1.Colon & Latin_1.Space;

   AMI_SOCKET_NOT_CONNECTED     : exception;
   AMI_SOCKET_NOT_AUTHENTICATED : exception;
   NOT_IMPLEMENTED              : exception;

   type Action_ID_Type is mod 2**16;

   Null_Action_ID : constant Action_ID_Type := -1;

end AMI;
