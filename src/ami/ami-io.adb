-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                  AMI.IO                                   --
--                                                                           --
--                                  BODY                                     --
--                                                                           --
--                     Copyright (C) 2012-, AdaHeads K/S                     --
--                                                                           --
--  This is free software;  you can redistribute it and/or modify it         --
--  under terms of the  GNU General Public License  as published by the      --
--  Free Software  Foundation;  either version 3,  or (at your  option) any  --
--  later version. This library is distributed in the hope that it will be   --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     --
--  You should have received a copy of the GNU General Public License and    --
--  a copy of the GCC Runtime Library Exception along with this program;     --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
--  <http://www.gnu.org/licenses/>.                                          --
--                                                                           --
-------------------------------------------------------------------------------

with Ada.Characters.Latin_1;
with AWS.Net.Buffered;
with Yolk.Log;
--  Provides I/O routines for reading from Asterisk AMI.
package body AMI.IO is
   function Read_Line (Socket : in AWS.Net.Std.Socket_Type) return String is
      Text : constant String := AWS.Net.Buffered.Get_Line (Socket => Socket);
   begin
      Yolk.Log.Trace (Yolk.Log.Debug, "IO: " & Text);
      return Text;
   end Read_Line;

   --  Reads until it catches an empty line.
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
      Yolk.Log.Trace (Yolk.Log.Debug, "OI: " & Item);
      AWS.Net.Buffered.Put (Socket, Item);
      AWS.Net.Buffered.Flush (Socket);
   end Send;
end AMI.IO;
