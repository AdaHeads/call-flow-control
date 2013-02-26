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

package body Access_Generator is
   Static_Object : aliased Instance := Instance'First;

   function Initialise return Reference is
   begin
      return Static_Object'Access;
   end Initialise;

   function Next return Reference is
   begin
      if Static_Object = Instance'Last then
         return null;
      else
         Static_Object := Static_Object + 1;
         return Static_Object'Access;
      end if;
   end Next;

   function Image (Item : in Instance) return String is
   begin
      return Instance'Image (Item);
   end Image;
end Access_Generator;
