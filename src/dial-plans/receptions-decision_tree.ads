limited
with Receptions.End_Point;

with Receptions.Action;

package Receptions.Decision_Tree is
   type Instance is abstract new Action.Instance with private;

   overriding
   function Application (Item : access Instance;
                         Call : in     Channel_ID) return
			 access Receptions.End_Point.Instance'Class;
private
   type Instance is abstract new Action.Instance with
      record
         Fall_Back : access Action.Class;
      end record;
end Receptions.Decision_Tree;
