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

package Receptions.End_Points.Redirect is
   type Instance is new End_Point.Instance with private;

   not overriding
   function Create (Title : in     String;
                    To    : in     String) return Instance;

   overriding
   function Title (Item : in     Instance) return String;

   overriding
   function Value (Item : in Instance) return String;

   not overriding
   function To (Item : in     Instance) return String;

   XML_Element_Name : constant String := "redirect";
private
   type Instance is new End_Point.Instance with
      record
         Title : Ada.Strings.Unbounded.Unbounded_String;
         To    : Ada.Strings.Unbounded.Unbounded_String;
      end record;
end Receptions.End_Points.Redirect;
