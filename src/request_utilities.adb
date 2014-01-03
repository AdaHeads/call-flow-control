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

with Model.Token.List,
     Model.User.List;

package body Request_Utilities is

   ----------------
   --  Token_Of  --
   ----------------

   function Token_Of (Request : in AWS.Status.Data)
                      return Model.Token.Instance is
      use AWS.Status;
      Token_String  : String renames Parameters (Request).Get ("token");
   begin
      return Model.Token.Create (Value => Token_String);
   end Token_Of;

   ---------------
   --  User_Of  --
   ---------------

   function User_Of (Request : in AWS.Status.Data)
                     return Model.User.Instance is
   begin
      return Model.User.List.Get_Singleton.Get
        (Identity => Model.Token.List.Get_Singleton.Look_Up
           (User_Token => Token_Of (Request => Request)));
   end User_Of;

end Request_Utilities;
