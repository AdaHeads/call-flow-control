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

--  Reponse handler for listing all calls visible to the user.
--
--  Parameters: None
--  Returns: A JSON response with the call list embedded.

with AWS.Response,
     AWS.Status;

package Handlers.Call.List is

   Package_Name : constant String := "Handlers.Call.List";

   function Callback return AWS.Response.Callback;

private
   function Generate_Response (Request : AWS.Status.Data)
                               return AWS.Response.Data;
end Handlers.Call.List;
