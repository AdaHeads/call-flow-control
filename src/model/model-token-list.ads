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

with GNATCOLL.JSON;

with Model.User;

package Model.Token.List is
   use GNATCOLL.JSON;

   Package_Name : constant String := "Model.Token.List";

   type Instance is tagged private;

   function Get_Singleton return Instance;

   function Look_Up (Object     : Instance;
                     User_Token : Token.Instance) return User.Identities;

   function To_JSON (Object : Instance) return JSON_Value;

private
   use Model;

   type Instance is tagged
      record
         Tokens : Token.Token_Maps := Token.Token_User_Storage.Empty_Map;
      end record;

end Model.Token.List;
