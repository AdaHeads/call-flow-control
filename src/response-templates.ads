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

package Response.Templates is

   Package_Name : constant String := "Response.Templates";

   function Bad_Parameters return Response.Object;
   --  Builds up a 400 Bad reponse. Used when invalid parameters or values are
   --  given along with a request.

   function Not_Authorized return Response.Object;
   --  Builds up a 401 Unauthorized response. Used when user validation fails,
   --  or they lack the proper authorization for a resource.

   function Not_Found return Response.Object;
   --  Builds up a 404 Not found response. Used as the default reponse handler
   --  for every request not in the routing table.

   function Server_Error return Response.Object;
   --  Builds up a 500 Server error object.

private
   Bad_Parameters_Reponse_Text : constant String := "bad parameters";
   Not_Authorized_Reponse_Text : constant String := "not authorized";
   Not_Found_Reponse_Text      : constant String := "not found";
   Server_Error_Reponse_Text   : constant String := "unhandled exception";

end Response.Templates;
