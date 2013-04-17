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
  Ada.Characters.Latin_1;

package body Phone_Numbers is
   function Is_Whitespace (Item : in     Character) return Boolean is
      use Ada.Characters.Latin_1;
   begin
      return Item in Space | No_Break_Space | HT;
   end Is_Whitespace;

   function Normalise (Item : in     String) return String is
      Buffer    : String (Item'Range);
      First     : Boolean := True;
      Filled_To : Natural := Buffer'First - 1;
   begin
      for C of Item loop
         if Is_Whitespace (C) then
            null; -- removing it
         elsif First and then C in '+' | '0' .. '9' then
            First := False;
            Filled_To := Filled_To + 1;
            Buffer (Filled_To) := C;
         elsif C in '0' .. '9' then
            Filled_To := Filled_To + 1;
            Buffer (Filled_To) := C;
         else
            return Item; -- not a (normal) phone number
         end if;
      end loop;

      return Buffer (Buffer'First .. Filled_To);
   end Normalise;

   function Same (Left, Right : in     String) return Boolean is
   begin
      return
        (Left = Right) or else
        (Normalise (Left) = Normalise (Right));
   end Same;
end Phone_Numbers;
