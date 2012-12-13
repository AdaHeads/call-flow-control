with
  Ada.Command_Line,
  Ada.Text_IO;

procedure Model.Channel_ID.Test is
   use Ada.Command_Line;

   procedure Image_Of_Value (Input  : in     String;
                             Output : in     String) is
      use Ada.Text_IO;
   begin
      declare
         Item : constant Instance := Value (Input);
      begin
         if Image (Item) = Output then
            null;
         else
            Put_Line ("""" & Input & """ becomes """ & Image (Item) &
                      """ and not """ & Output & """.");
            Set_Exit_Status (Failure);
         end if;
      end;
   exception
      when others =>
         Put_Line ("Conversion of """ & Input & """ raised an exception.");
         Set_Exit_Status (Failure);
   end Image_Of_Value;
begin
   Set_Exit_Status (Success);

   Image_Of_Value (Input  => "SIP/0004F2060EB4-0000001b",
                   Output => "SIP/0004F2060EB4-0000001B");

   Image_Of_Value (Input  => "SIP/JSA-N900-00000037",
                   Output => "SIP/JSA-N900-00000037");

   Image_Of_Value (Input  => "SIP/JSA-N900-00000035<zombie>",
                   Output => "<temporary>");

   Image_Of_Value (Input  => "GSM/JSA-N900-00000033",
                   Output => "<temporary>");
end Model.Channel_ID.Test;
