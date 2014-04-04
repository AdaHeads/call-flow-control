-------------------------------------------------------------------------------
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

with
  Black.HTTP;

package HTTP_Codes is

   OK                    : Black.HTTP.Statuses renames Black.HTTP.OK;
   No_Content            : Black.HTTP.Statuses renames Black.HTTP.No_Content;

   Bad_Request           : Black.HTTP.Statuses renames Black.HTTP.Bad_Request;
   Unauthorized          : Black.HTTP.Statuses renames Black.HTTP.Unauthorized;
   Forbidden             : Black.HTTP.Statuses renames Black.HTTP.Forbidden;
   Not_Found             : Black.HTTP.Statuses renames Black.HTTP.Not_Found;

   Server_Error          : Black.HTTP.Statuses renames Black.HTTP.Server_Error;
   Internal_Server_Error : Black.HTTP.Statuses renames Black.HTTP.Server_Error;

end HTTP_Codes;
