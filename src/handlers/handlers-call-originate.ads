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

--  Originates a new outbound call. This resource takes parameters
--
--  Parameters: Context and either phone_id or arbitrary extension.
--  Returns: HTTP 404 Not found and a JSON body if the call is not present.
--           HTTP 200 OK and a JSON body otherwise.
--
--  Where context Context refers to contact_id@reception_id

with Black.Request;

package Handlers.Call.Originate is

   Invalid_Extension : exception;

   Package_Name : constant String := "Handlers.Call.Originate";

   procedure Handle (Stream  : in     GNAT.Sockets.Stream_Access;
                     Request : in     Black.Request.Instance);
private
   Extension_String : constant String := "extension";
   Context_String   : constant String := "context";
   Phone_ID_String  : constant String := "phone_id";
end Handlers.Call.Originate;
