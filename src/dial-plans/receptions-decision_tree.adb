package body Receptions.Decision_Tree is
   overriding
   function Application (Item : access Instance;
                         Call : in     Channel_ID) return
			 access Receptions.End_Point.Instance'Class is
   begin
      return Application (Item.Fall_Back, Call);
   end Application;
end Receptions.Decision_Tree;
