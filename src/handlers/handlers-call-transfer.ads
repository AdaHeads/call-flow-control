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

--  Reponse handler for bridging a current call (active channel) to another
--  active call. This is used for performing attended transfers.
--
--  Typically this handler will bridge an active (as in speaking with a human)
--  call with a parked call.
--
--  Returns: HTTP 404 Not found and a JSON body any of the calls are not found.
--           HTTP 200 OK and a JSON body otherwise.

with HTTP,
     Black.Request,
     Black.Response;

with Request, Response;

package Handlers.Call.Transfer is
   package Client renames Black;
   package Server renames Black;

   Package_Name : constant String := "Handlers.Call.Transfer";

   function Callback return HTTP.Callback;

   function Handler (Client_Request : in Request.Instance)
                     return Response.Instance;

private
   function Generate_Response (Request : Black.Request.Instance)
                               return Server.Response.Class;

   Source_String      : constant String := "source";
   Destination_String : constant String := "destination";
   --  These are the required parameters.
end Handlers.Call.Transfer;
