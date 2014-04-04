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

with Black.Response,
     Black.Request;

package Handlers.Not_Found is

   function Callback
     return AWS.Dispatchers.Callback.Handler;
   --  Return a handler for the Not_Found (404) response.

   function Callback return Black.Response.Callback;
   --  Return a callback for the Not_Found (404) response.

private

   function Generate_Response (Request : Black.Request.Instance)
                               return Black.Response.Instance;
   --  Add a generated JSON_String to Response_Object and set HTTP status code
   --  to 404.
end Handlers.Not_Found;
