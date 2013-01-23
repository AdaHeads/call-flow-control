package body Receptions.End_Point is
   overriding
   function Application (Item : access Instance;
                         Call : in     Channel_ID) return
			 access Receptions.End_Point.Instance'Class is
   begin
      return Item;
   end Application;
end Receptions.End_Point;
