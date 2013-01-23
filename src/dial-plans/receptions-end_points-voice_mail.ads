with Receptions.End_Point;

private
with Ada.Strings.Unbounded;

package Receptions.End_Points.Voice_Mail is
   type Instance is new End_Point.Instance with private;

   function Play (Item : in     Instance) return String;

   function Send_To (Item : in     Instance) return String;
private
   type Instance is new End_Point.Instance with
      record
         Play    : Ada.Strings.Unbounded.Unbounded_String;
         Send_To : Ada.Strings.Unbounded.Unbounded_String;
      end record;
end Receptions.End_Points.Voice_Mail;
