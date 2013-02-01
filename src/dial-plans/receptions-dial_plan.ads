-------------------------------------------------------------------------------
--                                                                           --
--                      Copyright (C) 2012-, AdaHeads K/S                    --
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

limited
with Receptions.End_Point;

private
with Ada.Strings.Unbounded;

private
with Receptions.Action;

package Receptions.Dial_Plan is
   type Instance is private;

   function Title (Item : in     Instance) return String;

   function Application (Item : in     Instance;
                         Call : in     Channel_ID) return
			 access Receptions.End_Point.Instance'Class;
private
   type Instance is
      record
         Title : Ada.Strings.Unbounded.Unbounded_String;
         Start : access Receptions.Action.Class;
      end record;
end Receptions.Dial_Plan;
