with Receptions.End_Point;

private
with Ada.Strings.Unbounded;

package Receptions.End_Points.Queue is
   type Instance is new End_Point.Instance with private;

   function ID (Item : in     Instance) return String;
private
   type Instance is new End_Point.Instance with
      record
         ID : Ada.Strings.Unbounded.Unbounded_String;
      end record;
end Receptions.End_Points.Queue;
