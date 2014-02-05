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

package body Handlers.Call.List is

   ----------------
   --  Callback  --
   ----------------

   function Callback return AWS.Response.Callback is
   begin
      return Generate_Response'Access;
   end Callback;

   -------------------------
   --  Generate_Response  --
   -------------------------

   function Generate_Response (Request : AWS.Status.Data)
                               return AWS.Response.Data is
      Context : constant String := Package_Name & ".Generate_Response";
   begin
      return Response.Templates.OK (Request       => Request,
                                    Response_Body => Model.Call.List);
   exception
      when E : others =>
         System_Messages.Critical_Exception
           (Event           => E,
            Message         => "Listing calls failed.",
            Context => Context);
         return Response.Templates.Server_Error (Request);
   end Generate_Response;

end Handlers.Call.List;
