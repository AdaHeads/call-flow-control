with AWS.Net.Std;
with Ada.Strings.Unbounded;
package AMI.IO is
   use Ada.Strings.Unbounded;

   function Read_Line (Socket : in AWS.Net.Std.Socket_Type)
                       return String;
   --  reads a line (seperated by linefeed CRLF)

   function Read_Package (Socket : in AWS.Net.Std.Socket_Type)
                          return Unbounded_String;
   --  Returns a package.

   procedure Send (Socket : in AWS.Net.Std.Socket_Type;
                   Item   : in String);
end AMI.IO;
