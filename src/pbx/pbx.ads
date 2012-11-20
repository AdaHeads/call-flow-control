with AMI.Client;

package PBX is
   use AMI;

   Client        : aliased AMI.Client.Client_Type;
   Client_Access : constant access AMI.Client.Client_Type := Client'Access;

   procedure Start;

   procedure Stop;

   procedure Status;

private
   Shutdown : Boolean := False;

end PBX;
