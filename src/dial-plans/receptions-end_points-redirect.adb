package body Receptions.End_Points.Redirect is
   function To (Item : in     Instance) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Item.To);
   end To;
end Receptions.End_Points.Redirect;
