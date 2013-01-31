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

with Receptions.End_Point;

private
with Ada.Strings.Unbounded;

package Receptions.End_Points.Voice_Mail is
   type Instance is new End_Point.Instance with private;

   function Play (Item : in     Instance) return String;

   function Send_To (Item : in     Instance) return String;
private
   type Instance is new End_Point.Instance with
      record
         Play    : Ada.Strings.Unbounded.Unbounded_String;
         Send_To : Ada.Strings.Unbounded.Unbounded_String;
      end record;
end Receptions.End_Points.Voice_Mail;
