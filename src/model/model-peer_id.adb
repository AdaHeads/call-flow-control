with Common;

package body Model.Peer_ID is

   function "<" (Left  : in Peer_ID_Type;
                 Right : in Peer_ID_Type) return Boolean is
   begin
      return Left.Kind < Right.Kind and then
        Left.Peername < Right.Peername;
   end "<";

   function "=" (Left  : in Peer_ID_Type;
                 Right : in Peer_ID_Type) return Boolean is
   begin
      return Left.Kind = Right.Kind and Left.Peername = Right.Peername;
   end "=";

   function Create (Item : in String) return Peer_ID_Type is
      Kind_Offset    : constant Natural := Common.Index ('/', Item);
   begin
      if Kind_Offset > 2 then
         return
           Create (Channel_Kind =>
                     Item (Item'First .. Item'First + Kind_Offset - 2),
                   Peername     =>
                     Item (Item'First + Kind_Offset .. Item'Last));
      end if;
      raise Constraint_Error;
   exception
      when Constraint_Error =>
         raise Invalid_ID with "Invalid Peer ID: " & Item;
   end Create;

   function Create (Channel_Kind : in String;
                    Peername     : in String) return Peer_ID_Type is
   begin
      return (Kind     => Channel_Type'Value (Channel_Kind),
              Peername => To_Unbounded_String (Peername));
   end Create;

   function To_String (Peer_ID : in Peer_ID_Type) return String is
   begin
      if Peer_ID = Null_Peer_ID then
         return "<null>";
      else
         return Peer_ID.Kind'Img & "/" & To_String (Peer_ID.Peername);
      end if;
   end To_String;

end Model.Peer_ID;
