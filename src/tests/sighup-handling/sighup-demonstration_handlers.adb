with
  Ada.Text_IO;

package body SIGHUP.Demonstration_Handlers is
   procedure Minimal is
   begin
      Called := True;
   end Minimal;

   procedure Put_Line is
   begin
      Called := True;
      Ada.Text_IO.Put_Line ("Received a hangup signal.");
   end Put_Line;
begin
   SIGHUP.Register (Minimal'Access);
   SIGHUP.Register (Put_Line'Access);
end SIGHUP.Demonstration_Handlers;
