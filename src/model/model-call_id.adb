with Ada.Strings.Fixed;

with Common;

package body Model.Call_ID is

   function "<" (Left  : in Call_ID_Type;
                 Right : in Call_ID_Type) return Boolean is
   begin
      if Left.Timestamp = Right.Timestamp then
         return Left.Sequence < Right.Sequence;
      else
         return Left.Timestamp < Right.Timestamp;
      end if;
   end "<";

   function "=" (Left  : in Call_ID_Type;
                 Right : in Call_ID_Type) return Boolean is
   begin
      return (Left.Timestamp = Right.Timestamp) and
        (Left.Sequence = Right.Sequence);
   end  "=";

   function Create (Item : String) return Call_ID_Type is
      Offset : constant Natural := Common.Index ('.', Item);
   begin
      if Offset < 3 then
         return Null_Call_ID;
      else
         return
           (Timestamp => Integer'Value
              (Item (Item'First .. Item'First + Offset - 2)),
            Sequence  => Integer'Value
              (Item (Item'First + Offset .. Item'Last)));
      end if;
   end Create;

   function To_String (Call_ID : in Call_ID_Type) return String is
   begin
      if Call_ID = Null_Call_ID then
         return "<Null>";
      else
         return Ada.Strings.Fixed.Trim
           (Integer'Image (Call_ID.Timestamp),
            Ada.Strings.Left) &
           "." &
           Ada.Strings.Fixed.Trim
           (Integer'Image (Call_ID.Sequence),
            Ada.Strings.Left);
      end if;
   end To_String;
end Model.Call_ID;
