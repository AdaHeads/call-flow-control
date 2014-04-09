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

with
  Ada_2012.Strings.Unbounded.Equal_Case_Insensitive,
  Ada_2012.Strings.Unbounded.Hash_Case_Insensitive;

package body Model.Token is

   overriding
   function "=" (Left, Right : in Instance) return Boolean is
   begin
      return Ada_2012.Strings.Unbounded.Equal_Case_Insensitive
               (Left  => Left.Token_Value,
                Right => Right.Token_Value);
   end "=";

   function Create (Value : String) return Instance is
   begin
      return (Token_Value => To_Unbounded_String (Value));
   end Create;

   function Hash (Object : in Instance) return Ada.Containers.Hash_Type is
   begin
      return Ada_2012.Strings.Unbounded.Hash_Case_Insensitive
               (Object.Token_Value);
   end Hash;

   function To_String (Object : Instance) return String is
   begin
      return To_String (Object.Token_Value);
   end To_String;

end Model.Token;
