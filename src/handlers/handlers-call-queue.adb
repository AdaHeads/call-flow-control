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

with Model.Call,
     Response.Templates,
     System_Messages;

package body Handlers.Call.Queue is

   ----------------
   --  Callback  --
   ----------------

   function Callback return HTTP.Callback is
   begin
      return Generate_Response'Access;
   end Callback;

   -------------------------
   --  Generate_Response  --
   -------------------------

   function Generate_Response (Request : Black.Request.Instance)
                               return Black.Response.Class is
      Context : constant String := Package_Name & ".Generate_Response";
   begin
      return Response.Templates.OK
        (Request       => Request,
         Response_Body => Model.Call.Queued_Calls);
   exception
      when E : others =>
         System_Messages.Critical_Exception
           (Event           => E,
            Message         => "Listing queued calls failed.",
            Context => Context);
         return Response.Templates.Server_Error (Request);

   end Generate_Response;

   function Handler return Response.Instance is
   begin
      return Response.Create
        (Status      => Response.Internal_Error,
         Description => "Not implemented");
   end Handler;

end Handlers.Call.Queue;
