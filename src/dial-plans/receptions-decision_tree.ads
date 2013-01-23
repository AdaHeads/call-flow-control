with Receptions.Action;

package Receptions.Decision_Tree is
   type Instance is abstract new Action.Instance with private;

   overriding
   function Application (Item : access not null Instance;
                         Call : in     Channel_ID) return
			 access not null Receptions.End_Point.Instance'Class;
private
   type Instance is abstract new Action.Instance with
      record
         Fall_Back : access not null Action.Instance;
      end record;
end Receptions.Decision_Tree;
