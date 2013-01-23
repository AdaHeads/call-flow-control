limited
with Receptions.End_Point;

private
with Ada.Strings.Unbounded;

private
with Receptions.Action;

package Receptions.Dial_Plan is
   type Instance is private;

   function Title (Item : in     Instance) return String;

   function Application (Item : in     Instance;
                         Call : in     Channel_ID) return
			 access Receptions.End_Point.Instance'Class;
private
   type Instance is
      record
         Title : Ada.Strings.Unbounded.Unbounded_String;
         Start : access Receptions.Action.Class;
      end record;
end Receptions.Dial_Plan;
