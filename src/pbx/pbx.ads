with AMI.Client;
with Common;

package PBX is
   use AMI;

   type PBX_Status_Type is (Shutdown, Shutting_Down, Running, Connecting,
                           Failure);

   --  TODO: make this private and wrap every call to AMI
   Client        : aliased AMI.Client.Client_Type;
   Client_Access : constant access AMI.Client.Client_Type := Client'Access;

   procedure Start;
   --  Startup the PBX subsystem

   procedure Stop;
   --  Stop the PBX subsystem.

   function Status return PBX_Status_Type;
   --  Retrieve the current status of the

private
   Last_Connection_Attempt : Common.Time;
   Connection_Delay        : Duration        := 1.0;
   PBX_Status              : PBX_Status_Type := Failure;
end PBX;
