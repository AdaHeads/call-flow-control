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

--  https://github.com/AdaHeads/Call-Flow-Control/wiki/Protocol-Call-Originate
--
--  NOTE: Only the deprecated form of the interface is implemented.

with HTTP,
     Black.Request,
     Black.Response;

with Request, Response;

package Handlers.Call.Originate is

   Package_Name : constant String := "Handlers.Call.Originate";

   Invalid_Extension : exception;

   function Callback return HTTP.Callback;

   function Handler (Client_Request : in Request.Instance)
                     return Response.Instance;
   --  Expects the paramters reception_id, contact_id and extension.

private
   function Generate_Response (Request : Black.Request.Instance)
                               return Black.Response.Class;

   Extension_String     : constant String := "extension";
   Context_String       : constant String := "context";
   Phone_ID_String      : constant String := "phone_id";
   Contact_ID_String    : constant String := "contact_id";
   Reception_ID_String  : constant String := "reception_id";
end Handlers.Call.Originate;
