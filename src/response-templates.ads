-------------------------------------------------------------------------------
--                                                                           --
--                     Copyright (C) 2014-, AdaHeads K/S                     --
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

--  Ready-to-use responses with fixed content and response codes.

with Black.Request,
     Black.Response;
with GNATCOLL.JSON;

package Response.Templates is
   use GNATCOLL.JSON;

   Package_Name : constant String := "Response.Templates";

   function OK (Request       : in Black.Request.Instance;
                Response_Body : in JSON_Value := Create_Object)
                return Black.Response.Instance;

   function Bad_Parameters (Request : in Black.Request.Instance;
                            Response_Body : in JSON_Value := Create_Object)
                            return Black.Response.Instance;
   --  Builds up a 400 Bad reponse. Used when invalid parameters or values are
   --  given along with a request.

   function Not_Authorized (Request : in Black.Request.Instance)
                            return Black.Response.Instance;
   --  Builds up a 401 Unauthorized response. Used when user validation fails,
   --  or they lack the proper authorization for a resource.

   function Not_Found (Request       : in Black.Request.Instance;
                       Response_Body : in JSON_Value := Create_Object)
                       return Black.Response.Instance;
   --  Builds up a 404 Not found response. Used as the default reponse handler
   --  for every request not in the routing table.

   function Server_Error (Request       : in Black.Request.Instance;
                          Response_Body : in JSON_Value := Create_Object)
                          return Black.Response.Instance;
   --  Builds up a 500 Server error object.

   function Server_Error (Response_Body : in JSON_Value := Create_Object)
                          return Black.Response.Instance;
   --  Builds up a 500 Server error object.

private
end Response.Templates;
