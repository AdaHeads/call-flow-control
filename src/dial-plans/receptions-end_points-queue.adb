package body Receptions.End_Points.Queue is
   function ID (Item : in     Instance) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Item.ID);
   end ID;
end Receptions.End_Points.Queue;
