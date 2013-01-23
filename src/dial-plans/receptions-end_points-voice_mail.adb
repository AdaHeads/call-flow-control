package body Receptions.End_Points.Voice_Mail is
   function Play (Item : in     Instance) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Item.Play);
   end Play;

   function Send_To (Item : in     Instance) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Item.Send_To);
   end Send_To;
end Receptions.End_Points.Voice_Mail;
