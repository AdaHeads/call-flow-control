with Receptions.Action;

package body Receptions.Dial_Plan is
   function Title (Item : in     Instance) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Item.Title);
   end Title;

   function Application (Item : in     Instance;
                         Call : in     Channel_ID) return
			 access Receptions.End_Point.Instance'Class is
      use Receptions.Action;
   begin
      return Application (Item.Start, Call);
   end Application;
end Receptions.Dial_Plan;
