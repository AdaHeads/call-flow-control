-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                  AMI.IO                                   --
--                                                                           --
--                                  SPEC                                     --
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

with Ada.Strings.Unbounded;
with AWS.Net.Std;

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
