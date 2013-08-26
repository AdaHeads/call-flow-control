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

with AWS.Dispatchers.Callback;

with Model.User,
     Response.Not_Cached;

package Handlers.Configuration is

   function Callback
     return AWS.Dispatchers.Callback.Handler;
   --  Return a callback for the configuration interface.

private

   procedure Generate_Document
     (Instance : in out Response.Object);
   --  Add a generated JSON_String to Response_Object.

   function JSON_Response is new Response.Not_Cached.Generate_Response
     (Allowed           => (Model.User.Receptionist  => True,
                            Model.User.Service_Agent => False,
                            Model.User.Administrator => False),
      Generate_Document => Generate_Document);
   --  Generate the AWS.Response.Data that ultimately is delivered to the user.

end Handlers.Configuration;
