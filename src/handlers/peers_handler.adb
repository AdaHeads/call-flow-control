with Peers;
Package body Peers_handler is
   use Peers;

   Procedure Registate (Request : in AWS.Status.Data) is
      SIP_Name : String
      Peer : Peer_Type with Peer => SIP_Name;
   begin
      Peers.Insert_Peer (Peer);
   end Registate;
end Peers_handler;
