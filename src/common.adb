-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                 Common                                    --
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
with Ada.Calendar.Conversions;
with Ada.Strings.Fixed;
with Ada.Strings;
with Interfaces.C;

package body Common is
   function Unix_Timestamp
     (Date : in Time)
     return String
   is
      use Ada.Strings;
      use Interfaces.C;
   begin
      return Fixed.Trim
        (Source => long'Image 
           (Ada.Calendar.Conversions.To_Unix_Time (Date)),
         Side   => Left);
   end Unix_Timestamp;

   function Index (Char : Character;
                   Item : String) return Natural is
   begin
      for I in Item'range loop
         if Item (I) = Char then
            return I;
         end if;
      end loop;
      return 0;
   end Index;
end Common;   
