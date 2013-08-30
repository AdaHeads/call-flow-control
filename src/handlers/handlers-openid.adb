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
     AWS.OpenID.Manual_Dispatching,
     Yolk.Configuration;

with Alice_Configuration,
     System_Message.Critical;

package body Handlers.OpenID is

   use AWS.OpenID;

   function Protocol return String;
   function Host_Name return String;

   function Host_Name return String is
      use Yolk.Configuration, Alice_Configuration;
   begin
      return Alice_Configuration.Config.Get (Host_Name) &
        ":" & Yolk.Configuration.Config.Get (Server_Port);
   end Host_Name;

   function Protocol return String is
      use Yolk.Configuration;
   begin
      if Config.Get (Security) then
         return "https://";
      else
         return "http://";
      end if;
   exception
      when others =>
         System_Message.Critical.Configuration_Error
           (Message => "'Security' is a Boolean.  """ &
                       Config.Get (Security) & """ is not a valid choice.");
         raise;
   end Protocol;

   package OpenID is new Manual_Dispatching
     (Authentication_Failed => Error_Messages.Authentication_Failed'Access,
      Invalid_End_Point     => Error_Messages.Invalid_End_Point'Access,
      Invalid_URL           => Error_Messages.Invalid_URL'Access,
      Provider_Offline      => Error_Messages.Provider_Offline'Access,
      Protocol              => Protocol,
      Host_Name             => Host_Name,
      Log_In_Page           => "/users/log_in",
      Return_To_Page        => "/users/validate",
      Logged_In_Page        => "/users/logged_in",
      Log_Out_Page          => "/users/log_out",
      Logged_Out_Page       => "/users/logged_out");

   function Log_In (Request : in     AWS.Status.Data)
                   return AWS.Response.Data is
   begin
      if Alice_Configuration.Unsafe_Mode then
         System_Message.Critical.Running_In_Unsafe_Mode
           (Message => "Handlers.OpenID: OpenID authentication inactive.");

         return AWS.Response.URL ("http://bob.adaheads.com/bob_webui.html");
      else
         return OpenID.Log_In.Service (Request);
      end if;
   end Log_In;

   function Log_Out (Request : in     AWS.Status.Data)
                    return AWS.Response.Data is
   begin
      return OpenID.Log_Out.Service (Request);
   end Log_Out;

   function Permissions (Request : in     AWS.Status.Data)
                        return Model.User.Permission_List is
   begin
      if Alice_Configuration.Unsafe_Mode then
         System_Message.Critical.Running_In_Unsafe_Mode
           (Message => "Handlers.OpenID: Returning fake authorizations.");

         return (others => True);
      elsif OpenID.Is_Authenticated (Request) then
         return Model.User.Permissions (Model.User.Parse
                                          (OpenID.Authenticated_As (Request)));
      else
         return (others => False);
      end if;
   end Permissions;

   function Validate (Request : in     AWS.Status.Data)
                   return AWS.Response.Data is
   begin
      if Alice_Configuration.Unsafe_Mode then
         System_Message.Critical.Running_In_Unsafe_Mode
           (Message => "Handlers.OpenID: Not validating OpenID " &
                       "authentication.");

         return AWS.Response.URL ("http://bob.adaheads.com/bob_webui.html");
      else
         return OpenID.Validate.Service (Request);
      end if;
   end Validate;

end Handlers.OpenID;
