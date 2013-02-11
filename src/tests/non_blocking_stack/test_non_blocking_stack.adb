-------------------------------------------------------------------------------
--                                                                           --
--                     Copyright (C) 2013-, AdaHeads K/S                     --
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
     Ada.Text_IO;

with Non_Blocking_Stack;

procedure Test_Non_Blocking_Stack is
   package Test_Stack is new Non_Blocking_Stack (Element_Type => Integer);

   use Ada.Command_Line, Ada.Text_IO;

   Stack   : Test_Stack.Instance;
   Element : Integer;
begin
   Stack.Get (Item => Element, Default => 42);
   Put_Line ("Empty stack didn't block.");

   if Element = 42 then
      Put_Line ("Empty stack returned default value.");
   else
      Put_Line ("Empty stack returned non-default value.");
      Set_Exit_Status (Failure);
      return;
   end if;

   for Test_Value in 10 .. 14 loop
      Stack.Push (Test_Value);
   end loop;
   Put_Line ("Stack received new values.");

   for Test_Value in reverse 10 .. 14 loop
      Stack.Get (Element, 42);
      if Element = Test_Value then
         Put_Line ("Stack returned value in (reverse) input order.");
      else
         Put_Line ("Stack returned value out of (reverse) input order.");
         Set_Exit_Status (Failure);
         return;
      end if;
   end loop;
end Test_Non_Blocking_Stack;
