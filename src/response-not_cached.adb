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

with System_Messages;
private with Response.Templates;

package body Response.Not_Cached is

   -------------------------
   --  Generate_Response  --
   -------------------------

   function Generate_Response
     (Request : in AWS.Status.Data)
         return AWS.Response.Data
   is
      Context         : constant String := Package_Name & ".Generate_Response";

      Response_Object : Object := Factory (Request);
   begin
      Generate_Document (Response_Object);

      return Response_Object.Build;
   exception
      when Event : others =>
         --  For now we assume that "other" exceptions caught here are bad
         --  enough to warrant a critical level log entry and response.
         System_Messages.Critical_Exception
           (Event   => Event,
            Message => Response_Object.To_Debug_String,
            Context => Context);
         return Response.Templates.Server_Error.Build;

   end Generate_Response;

end Response.Not_Cached;
