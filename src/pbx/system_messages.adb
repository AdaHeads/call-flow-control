with Ada.Text_IO;

package body System_Messages is

   procedure Notify (Level : in Message_Type; Message : in String) is
   begin
      Ada.Text_IO.Put_Line (Message_Type'Image (level) & 
			      ": " & message);
   end Notify;
end System_Messages;
