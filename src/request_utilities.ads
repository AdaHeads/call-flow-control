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

--  Utility package for extracting various useful information out of a requst.

with AWS.Status;

with Model.Token,
     Model.User;

package Request_Utilities is

   Package_Name : constant String := "Request_Utilities";

   function Token_Of (Request : in AWS.Status.Data)
                      return Model.Token.Instance;
   --  Returns the token associated with the current request.

   function User_Of (Request : in AWS.Status.Data)
                     return Model.User.Instance;
   --  Returns the user associated with the current request.

end Request_Utilities;
