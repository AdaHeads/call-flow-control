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

package body Non_Blocking_Stack is

   ----------------
   --  Instance  --
   ----------------

   protected body Instance is

      ------------
      --  Push  --
      ------------

      procedure Push
        (Item : in Element_Type)
      is
      begin
         Data.Append (Item);
         Empty := False;
      end Push;

      -----------
      --  Get  --
      -----------

      procedure Get
        (Item    :    out Element_Type;
         Default : in     Element_Type)
      is
      begin
         if Empty then
            Item := Default;
         else
            Item := Data.Last_Element;
            Data.Delete_Last;
            Empty := Data.Is_Empty;
         end if;
      end Get;

      ----------------
      --  Is_Empty  --
      ----------------

      function Is_Empty
        return Boolean
      is
      begin
         return Empty;
      end Is_Empty;

   end Instance;

end Non_Blocking_Stack;
