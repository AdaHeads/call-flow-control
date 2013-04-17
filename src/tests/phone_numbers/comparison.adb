-------------------------------------------------------------------------------
--                                                                           --
--                      Copyright (C) 2013-, AdaHeads K/S                    --
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

with
  Ada.Command_Line;
with
  Phone_Numbers;
with Ada.Text_IO;

procedure Comparison is
   use Ada.Command_Line;
   use Phone_Numbers;
begin
   if Argument_Count = 2 and then Same (Argument (1),
                                        Argument (2)) then
      Set_Exit_Status (Success);
   elsif Argument_Count /= 2 then
Ada.Text_IO.Put_Line (Natural'Image (Argument_Count));
      Set_Exit_Status (Failure);
   else
      Set_Exit_Status (Failure);
   end if;
end Comparison;
