-------------------------------------------------------------------------------
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
package AMI is

   package Latin_1 renames Ada.Characters.Latin_1;

   Line_Termination_String : constant String := (1 => ASCII.CR, 2 => ASCII.LF);

   Peer_State_Unregistered : constant String := "Unregistered";
   Peer_State_Registered : constant String := "Registered";

   Packet_Termination_String : constant String :=
     Line_Termination_String & Line_Termination_String;

   Key_Value_Seperator : constant String := ":";

   AMI_SOCKET_NOT_CONNECTED     : exception;
   AMI_SOCKET_NOT_AUTHENTICATED : exception;
   NOT_IMPLEMENTED              : exception;

   type Action_ID_Type is mod 2**16;

   Null_Action_ID : constant Action_ID_Type := -1;

end AMI;
