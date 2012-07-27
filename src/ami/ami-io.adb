with Ada.Characters.Latin_1;
with AWS.Net.Buffered;
--  Provides I/O routines for reading from Asterisk AMI.
package body AMI.IO is
   function Read_Line (Socket : in AWS.Net.Std.Socket_Type) return String is
      Text : constant String := AWS.Net.Buffered.Get_Line (Socket => Socket);
   begin
      return Text;
   end Read_Line;

   function Read_Package (Socket : in AWS.Net.Std.Socket_Type)
                          return Unbounded_String is

      package Char renames Ada.Characters.Latin_1;

      Newline : constant String := Char.CR & Char.LF;
      Buffer  : Unbounded_String;
   begin
      Collecting_Package :
      loop
         declare
            Line : constant String := Read_Line (Socket);
         begin
            exit Collecting_Package when Line = "";
            Append (Buffer, Line & Newline);
         end;

      end loop Collecting_Package;
      return Buffer;
   end Read_Package;

   procedure Send (Socket : in AWS.Net.Std.Socket_Type;
                   Item   : in String) is
   begin
      AWS.Net.Buffered.Put (Socket, Item);
      AWS.Net.Buffered.Flush (Socket);
   end Send;
end AMI.IO;
