with Common;
with System_Messages;

package body Model.Peer_ID is
   use System_Messages;

   function "<" (Left  : in Peer_ID_Type;
                 Right : in Peer_ID_Type) return Boolean is
   begin
      return Left.Class < Right.Class and then
        Left.Peername < Right.Peername;
   end "<";

   function "=" (Left  : in Peer_ID_Type;
                 Right : in Peer_ID_Type) return Boolean is
   begin
      return Left.Class = Right.Class and Left.Peername = Right.Peername;
   end "=";

   function Create (Item : in String) return Peer_ID_Type is
      Class_Offset    : constant Natural := Common.Index ('/', Item);
   begin
      if Class_Offset > 2 then
         return
           (Class    => Channel_Type'Value
              (Item (Item'First .. Item'First + Class_Offset - 2)),
            Peername =>
              To_Unbounded_String
                (Item (Item'First + Class_Offset .. Item'Last)));
      end if;
      System_Messages.Notify (Debug, "Model.Peer_ID.Create: Returning Null!");
      return Null_Peer_ID;
   end Create;

   function To_String (Peer_ID : in Peer_ID_Type) return String is
   begin
      if Peer_ID = Null_Peer_ID then
         return "<null>";
      else
         return Peer_ID.Class'Img & "/" & To_String (Peer_ID.Peername);
      end if;
   end To_String;

end Model.Peer_ID;
