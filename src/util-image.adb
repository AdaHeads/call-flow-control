-------------------------------------------------------------------------------
--                                                                           --
--                     Copyright (C) 2014-, AdaHeads K/S                     --
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

with Ada.Strings.Fixed;

package body Util.Image is
   use Ada.Strings;
   use Ada.Strings.Fixed;
   use Model;

   function Image (ID : Model.Identifications) return String is
   begin
      return Trim_Left (Identifications'Image (ID));
   end Image;

   function Trim_Both (Item : in String) return String is
   begin
      return Trim (Item, Both);
   end Trim_Both;

   function Trim_Left (Item : in String) return String is
   begin
      return Trim (Item, Left);
   end Trim_Left;

   function Trim_Right (Item : in String) return String is
   begin
      return Trim (Item, Right);
   end Trim_Right;

end Util.Image;
