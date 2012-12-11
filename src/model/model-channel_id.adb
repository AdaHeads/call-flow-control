with Ada.Strings.Fixed;

package body Model.Channel_ID is

   function "<" (Left  : in Channel_ID_Type;
                 Right : in Channel_ID_Type) return Boolean is
   begin
      --  if Left.Class = Right.Class then
         return Left.Sequence < Right.Sequence;
      --  end if;
   end "<";

   function "=" (Left  : in Channel_ID_Type;
                 Right : in Channel_ID_Type) return Boolean is
   begin
      --  return (Left.Timestamp = Right.Timestamp) and
      return  (Left.Sequence = Right.Sequence);
   end  "=";

   function Create (Item : String) return Channel_ID_Type is
      Class_Offset    : constant Natural := Ada.Strings.Fixed.Index (Pattern => "/",
                                                                     Source  => Item);
      Peername_Offset : constant Natural := Ada.Strings.Fixed.Index (Pattern => "-",
                                                                     Source  => Item,
                                                                     Going   => Ada.Strings.Backward);
   begin
      if Class_Offset < 3 then
         return Null_Channel_ID;
      else
         return
           (Kind => Technology'Value
              (Item (Item'First .. Item'First + Class_Offset - 2)),
            Peername =>
              To_Unbounded_String
                (Item (Item'First + Class_Offset ..
                   Item'First + Peername_Offset - 2)),
            Sequence =>
              (Item (Item'First + Peername_Offset .. Item'Last)));
      end if;
   end Create;

   function To_String (Channel_ID : in Channel_ID_Type) return String is
   begin
      if Channel_ID = Null_Channel_ID then
         return "<Null>";
      else
         return
           Technology'Image (Channel_ID.Kind) &
           "/" &
           To_String (Channel_ID.Peername) &
           "-" &
           Channel_ID.Sequence;
      end if;
   end To_String;
end Model.Channel_ID;
