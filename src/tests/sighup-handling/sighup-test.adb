with
  Ada.Command_Line,
  Ada.Text_IO;
with
  SIGHUP.Demonstration_Handlers;

procedure SIGHUP.Test is
begin
   Ada.Text_IO.Put_Line ("Waiting 10 seconds.");
   delay 10.0;
   Ada.Text_IO.Put_Line ("Stopping the SIGHUP handlers.");
   SIGHUP.Stop;

   if SIGHUP.Demonstration_Handlers.Called then
      Ada.Text_IO.Put_Line ("At least one of the demonstration handlers were called.");
   else
      Ada.Text_IO.Put_Line ("None of the demonstration handlers were called.");
   end if;

   Ada.Text_IO.Put_Line ("Test done.");
end SIGHUP.Test;
