-------------------------------------------------------------------------------
--                                                                           --
--                     Copyright (C) 2013-, AdaHeads K/S                     --
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

with AWS.OpenID.Error_Messages,
     AWS.OpenID.Manual_Dispatching;

with Alice_Configuration;

package body Handlers.OpenID is

   use AWS.OpenID;

   function Host_Name return String;
   function Host_Name return String is
      use Alice_Configuration;
   begin
      return Config.Get (Host_Name);
   end Host_Name;

   package OpenID is new Manual_Dispatching
     (Authentication_Failed => Error_Messages.Authentication_Failed'Access,
      Invalid_End_Point     => Error_Messages.Invalid_End_Point'Access,
      Invalid_URL           => Error_Messages.Invalid_URL'Access,
      Provider_Offline      => Error_Messages.Provider_Offline'Access,
      Host_Name             => Host_Name,
      Log_In_Page           => "/users/log_in",
      Return_To_Page        => "/users/validate");

   function Log_In (Request : in     AWS.Status.Data)
                   return AWS.Response.Data is
   begin
      return OpenID.Log_In.Service (Request);
   end Log_In;

end Handlers.OpenID;
