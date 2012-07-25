with Ada.Text_IO;
with Socket;
with AWS.Net.Std;
with AWS.Net.Buffered;
with Ada.Strings.Unbounded;
package body AMI is
   Channel : AWS.Net.Std.Socket_Type;
   --  it has package scope because, we need it in the Terminate_AMI procedure.

   task AMI_Service is
      entry Start (Username : in String;
                   Secret : in String);
   end AMI_Service;

   task body AMI_Service is
      use Ada.Strings.Unbounded;
      Username_Unbounded : Unbounded_String;
      Secret_Unbounded : Unbounded_String;
   begin
      accept Start (Username : in String;
                    Secret : in String) do
         Username_Unbounded := To_Unbounded_String (Username);
         Secret_Unbounded := To_Unbounded_String (Secret);
      end Start;

      Socket.Start (Channel, To_String (Username_Unbounded),
                            To_String (Secret_Unbounded));

   exception
      when others =>
         Ada.Text_IO.Put_Line ("Exception in AMI: ");
   end AMI_Service;

   procedure Connect (Server_Host : in String := "Asterisk1";
                      Server_Port : in Positive := 5038;
                      Username : in String := "test";
                      Secret : in String := "test") is
   begin
      AWS.Net.Std.Connect (Socket => Channel,
                           Host => Server_Host,
                           Port => Server_Port);

      AMI_Service.Start (Username, Secret);
   end Connect;

   procedure Terminate_AMI is
      use Ada.Text_IO;
   begin
      AWS.Net.Buffered.Shutdown (Channel);
   end Terminate_AMI;
end AMI;
