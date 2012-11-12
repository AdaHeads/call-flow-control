-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                  Model                                    --
--                                                                           --
--                                  SPEC                                     --
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

with Ada.Containers;
with Ada.Strings.Unbounded;

package Model is

   use Ada.Strings.Unbounded;

   type Contact_Identifier is mod 2**31 - 1;
   type Organization_Identifier is mod 2 ** 31 - 1;

   function Equivalent_Keys
     (Left, Right : in Unbounded_String)
      return Boolean;
   --  Key equality function used by the Attributes_Map.

   function Key_Hash
     (Key : in Unbounded_String)
      return Ada.Containers.Hash_Type;

   function Map_Key
     (C_Id : in Contact_Identifier;
      O_Id : in Organization_Identifier)
      return Unbounded_String;

end Model;
