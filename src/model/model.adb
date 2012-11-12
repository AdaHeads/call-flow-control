-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                  Model                                    --
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

with Ada.Strings.Hash;
with Common;

package body Model is

   -----------------------
   --  Equivalent_Keys  --
   -----------------------

   function Equivalent_Keys
     (Left, Right : in Unbounded_String)
      return Boolean
   is
   begin
      return Left = Right;
   end Equivalent_Keys;

   ----------------
   --  Key_Hash  --
   ----------------

   function Key_Hash
     (Key : in Unbounded_String)
      return Ada.Containers.Hash_Type
   is
   begin
      return Ada.Strings.Hash (To_String (Key));
   end Key_Hash;

   ---------------
   --  Map_Key  --
   ---------------

   function Map_Key
     (C_Id : in Contact_Identifier;
      O_Id : in Organization_Identifier)
      return Unbounded_String
   is
      use Common;
   begin
      return U (Contact_Identifier'Image (C_Id) &
                  Organization_Identifier'Image (O_Id));
   end Map_Key;

end Model;
