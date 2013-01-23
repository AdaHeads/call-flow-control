package body Receptions.Action is
   function Title (Item : in     Class) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Item.Title);
   end Title;
end Receptions.Action;
