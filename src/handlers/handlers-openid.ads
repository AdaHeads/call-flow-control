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

with AWS.Response,
     AWS.Status;

with Model.User;

private
package Handlers.OpenID is

   function Log_In (Request : in     AWS.Status.Data)
                   return AWS.Response.Data;

   function Validate (Request : in     AWS.Status.Data)
                     return AWS.Response.Data;

   function Log_Out (Request : in     AWS.Status.Data)
                    return AWS.Response.Data;

   function Permissions (Request : in     AWS.Status.Data)
                        return Model.User.Permission_List;

end Handlers.OpenID;
