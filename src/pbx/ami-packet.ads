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

package AMI.Packet is

   package Latin_1 renames Ada.Characters.Latin_1;

   type AMI_Packet is new String;
   type AMI_Line is new String;

   Line_Termination_String : constant String := Latin_1.CR & Latin_1.LF;
   Separator               : constant String := ": ";

   function Next return Action_ID_Type;

private
   Current_Action_ID : Action_ID_Type := Action_ID_Type'First;
   pragma Atomic (Current_Action_ID);
end AMI.Packet;
