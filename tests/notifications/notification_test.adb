with Ada.Command_Line;

with GNATCOLL.JSON;

with Configuration,
     Notification;

procedure Notification_Test is
   function Run return Boolean;
   function Run return Boolean is
      use Ada.Command_Line;
   begin
      for Index in 1 .. Argument_Count loop
         if Argument (Index) = "--no-run" then
            return False;
         end if;
      end loop;
      return True;
   end Run;
begin
   if Run then
      Configuration.Load_Config;

      declare
         use GNATCOLL.JSON;
         Message : constant JSON_Value := Create_Object;
      begin
         Message.Set_Field ("event", "call_offer");
         Notification.Broadcast (Message);
      end;
   end if;
end Notification_Test;
