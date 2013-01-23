with Receptions.End_Point;

private
with Ada.Strings.Unbounded;

package Receptions.End_Points.Redirect is
   type Instance is new End_Point.Instance with private;

   function To (Item : in     Instance) return String;
private
   type Instance is new End_Point.Instance with
      record
         To : Ada.Strings.Unbounded.Unbounded_String;
      end record;
end Receptions.End_Points.Redirect;
