with Ada.Strings.Unbounded;
with Ada.Characters.Handling;

package body View.Peer is
   use Ada.Strings.Unbounded;
   use Ada.Characters.Handling;

   function To_JSON (Peer : in Peers.Peer_Type)
                     return GNATCOLL.JSON.JSON_Value is
      use GNATCOLL.JSON;

      JSON      : constant JSON_Value := Create_Object;
      Peer_JSON : constant JSON_Value := Create_Object;
   begin
      Peer_JSON.Set_Field ("ID", Peer.ID.To_String);
      Peer_JSON.Set_Field ("Agent_ID", Peer.Agent_ID.To_String);
      Peer_JSON.Set_Field ("State", To_Lower (Peer.State'Img));
      Peer_JSON.Set_Field ("Last_State", To_Lower (Peer.Last_State'Img));
      Peer_JSON.Set_Field ("Port", To_String (Peer.Port));
      Peer_JSON.Set_Field ("Address", To_String (Peer.Address));
      Peer_JSON.Set_Field ("Last_Seen", Peers.To_String (Peer.Last_Seen));
      JSON.Set_Field ("peer", Peer_JSON);

      return JSON;
   end To_JSON;

end View.Peer;
