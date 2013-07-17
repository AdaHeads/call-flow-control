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

with Ada.Command_Line,
     Ada.Containers.Indefinite_Vectors,
     Ada.Text_IO;

procedure Container_Loop is
   use Ada.Command_Line, Ada.Text_IO;

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Element_Type => String,
                                            Index_Type   => Positive);
   use String_Vectors;

   function Example (Item : in String) return Vector is
   begin
      return Result : Vector do
         Result.Append (Item);
      end return;
   end Example;
begin
   for E of Example (Argument (1)) loop
      Put_Line (E);
   end loop;
end Container_Loop;
