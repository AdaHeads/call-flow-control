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

with Ada.Containers.Vectors;

generic

   type Element_Type is private;

package Non_Blocking_Stack is

   package Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Element_Type);

   protected type Instance is
      procedure Push
        (Item : in Element_Type);
      --  Append Item to the stack.

      procedure Get
        (Item    :    out Element_Type;
         Default : in     Element_Type);
      --  Get Item from the stack. Get Default if there are no elements in the
      --  stack.

      function Is_Empty
        return Boolean;
      --  Return True if the stack is empty.
   private
      Data  : Vectors.Vector;
      Empty : Boolean := True;
   end Instance;

end Non_Blocking_Stack;
