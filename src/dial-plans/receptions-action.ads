limited
with Receptions.End_Point;

private
with Ada.Strings.Unbounded;

package Receptions.Action is
   type Instance is abstract tagged private;
   subtype Class is Instance'Class;

   function Title (Item : in     Class) return String;

   function Application (Item : access Instance;
                         Call : in     Channel_ID) return
			 access Receptions.End_Point.Instance'Class is abstract;
private
   type Instance is abstract tagged
      record
         Title : Ada.Strings.Unbounded.Unbounded_String;
      end record;
end Receptions.Action;
