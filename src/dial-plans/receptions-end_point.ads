with Receptions.Action;

package Receptions.End_Point is
   type Instance is abstract new Action.Instance with null record;
   subtype Class is Instance'Class;

   overriding
   function Application (Item : access Instance;
                         Call : in     Channel_ID) return
			 access Receptions.End_Point.Instance'Class;
end Receptions.End_Point;
