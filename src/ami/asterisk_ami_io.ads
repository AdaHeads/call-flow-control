--  with GNAT.Sockets;   use GNAT.Sockets;
with AWS.Net.Std;
with Ada.Strings.Unbounded;
package Asterisk_AMI_IO is
   use Ada.Strings.Unbounded;
   function Read_Line (Channel : in AWS.Net.Std.Socket_Type) return String;
   --  reads a line (seperated by linefeed CRLF)

   function Read_Package (Channel : in AWS.Net.Std.Socket_Type)
                          return Unbounded_String;
   --  Returns a package.
end Asterisk_AMI_IO;
