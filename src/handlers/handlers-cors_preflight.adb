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

with GNATCOLL.JSON;
with Response.Templates;

package body Handlers.CORS_Preflight is

   ---------------
   --  Callback --
   ---------------
   function Callback return HTTP.Callback is
   begin
      return Generate_Response'Access;
   end Callback;

   --  Return a callback for the OPTIONS CORS preflight response (200).

   function Generate_Response (Request : Black.Request.Instance)
                               return Black.Response.Instance'Class is
   begin
      return Response.Templates.OK
        (Request       => Request,
         Response_Body => GNATCOLL.JSON.Create_Object);
   end Generate_Response;

end Handlers.CORS_Preflight;
