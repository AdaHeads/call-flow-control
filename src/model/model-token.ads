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

with Ada.Containers.Hashed_Maps;
private with Ada.Strings.Unbounded;

with Model.User;

package Model.Token is
   use Model;

   type Instance is tagged private;

   function Create (Value : String) return Instance;

   function To_String (Object : Instance) return String;

   function "=" (Left, Right : in Instance) return Boolean;

private
   use Ada.Strings.Unbounded;

   type Instance is tagged
      record
         Token_Value : Unbounded_String;
      end record;

   function Hash (Object : in Instance) return Ada.Containers.Hash_Type;

   package Token_User_Storage is new Ada.Containers.Hashed_Maps
     (Key_Type        => Instance,
      Element_Type    => User.Identities,
      Hash            => Hash,
      Equivalent_Keys => "=");

   subtype Token_Maps is Token_User_Storage.Map;

end Model.Token;
