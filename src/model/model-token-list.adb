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

package body Model.Token.List is

   Tokens : Instance;

   function Get_Singleton return Instance is
   begin
      return Tokens;
   end Get_Singleton;

   function Look_Up (Object     : Instance;
                     User_Token : Token.Instance) return User.Identities is
   begin
      if User_Token.To_String = "1" then
         return "kim.rostgaard@gmail.com";
      elsif User_Token.To_String = "2" then
         return "devicesnull@gmail.com";
      end if;

      return "null";
   end Look_Up;

end Model.Token.List;
