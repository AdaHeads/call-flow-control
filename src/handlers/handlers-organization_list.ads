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

with AWS.Response;

with Response.Not_Cached;

private
package Handlers.Organization_List is

   function Callback
     return AWS.Response.Callback;
   --  Return a callback for the "organization_list" interface.

private

   procedure Generate_Document
     (Instance : in out Response.Object);
   --  Add a generated JSON_String to Instance and set HTTP status code and
   --  whether the JSON_String is cacheable.

   procedure Set_Request_Parameters
     (Instance : out Response.Object);
   --  Set required/optional GET/POST request parameters.

   function JSON_Response is new Response.Not_Cached.Generate_Response
     (Generate_Document => Generate_Document);
   --  Generate the AWS.Response.Data that ultimately is delivered to the user.

end Handlers.Organization_List;
