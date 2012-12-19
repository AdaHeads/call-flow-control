with
  Ada.Text_IO;

procedure SIGHUP.Test_Handler is
begin
   Ada.Text_IO.Put_Line ("Received a hangup signal.");
end SIGHUP.Test_Handler;
