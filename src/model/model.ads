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

package Model is

   type Contact_Identifier is mod 2 ** 31 - 1;
   type Organization_Identifier is mod 2 ** 31 - 1;

   type Contact_Key is
      record
         C_Id : Contact_Identifier;
         O_Id : Organization_Identifier := 0;
      end record;
   --  This type is used as the key for the hashed maps used throughout the
   --  Model packages.

   function Equivalent_Keys
     (Left, Right : in Contact_Key)
      return Boolean;
   --  Key equivalence function used by hashed maps.

   function Key_Hash
     (Key : in Contact_Key)
      return Ada.Containers.Hash_Type;
   --  Hashing function used by the hashed maps.

end Model;
