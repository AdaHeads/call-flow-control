with AMI; 

package PBX is
   use AMI; 
   
   Client        : aliased Client_Type;
   Client_Access : constant access Client_Type := Client'Access;
   
   procedure Start;
   
   procedure Stop;
   
   procedure Status;
   
private 
   Shutdown : Boolean := False;
   
end PBX;
