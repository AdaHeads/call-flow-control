with
  SIGHUP.Test_Handler;

procedure SIGHUP.Test is
begin
   SIGHUP.Register (Test_Handler'Access);
   delay 10.0;
   SIGHUP.Stop;
end SIGHUP.Test;
