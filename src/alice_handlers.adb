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

with AWS.Net.WebSocket.Registry,
     AWS.Services.Dispatchers.Method,
     AWS.Services.Dispatchers.URI;

with Yolk.Static_Content;

with Alice_Configuration,
     Handlers.Authenticated_Dispatcher,
     Model.User;

with CORS_Preflight,
     Handlers.Agent,
     Handlers.Call,
     Handlers.Configuration,
     Handlers.Contact,
     Handlers.Debug,
     Handlers.Log,
     Handlers.Notifications,
     Handlers.Organization,
     Handlers.Organization_List,
     Handlers.Users.List,
     Handlers.Users.Log_In,
     Handlers.Users.Log_Out,
     Handlers.Users.Logged_In,
     Handlers.Users.Logged_Out,
     Handlers.Users.OpenIDs,
     Handlers.Users.Validate,
     Not_Found;

package body Alice_Handlers is

   function Callback (Request : in AWS.Status.Data) return AWS.Response.Data is
   begin
      return Handlers.Authenticated_Dispatcher.Run (Request);
   end Callback;

   -----------
   --  Get  --
   -----------

   function Get_Obsolecent
     return AWS.Services.Dispatchers.Method.Handler;
   function Get_Obsolecent
     return AWS.Services.Dispatchers.Method.Handler
   is
      use AWS.Services;
      use Handlers;

      Method_Dispatcher   : Dispatchers.Method.Handler;
      URI_GET_Dispatcher  : Dispatchers.URI.Handler;
   begin
      -----------------
      --  Bob files  --
      -----------------

      Yolk.Static_Content.Set_Cache_Options;
      --  Set some basic cache options for the static content.

      URI_GET_Dispatcher.Register_Regexp
        (URI    => "/bob/*.",
         Action => Yolk.Static_Content.Non_Compressable'Access);
      --  If a request begins with /bob/, then look for a file matching the
      --  given URI, sans domain, postfixed the WWW_Root configuration
      --  parameter.
      --  We keep this as simple as possible. If things like compressing and
      --  serverside caching of files is necessary, consider using an actual
      --  proxy / cache server like varnish in front of Alice.

      return Method_Dispatcher;
   end Get_Obsolecent;

   package Permission_Operations is
      use Handlers.Authenticated_Dispatcher, Model;

      Public        : constant Authentication := (Public => True);
      Logged_In     : constant Authentication := (Public => False,
                                                  As     => (others => True));
      Receptionist  : constant Authentication :=
        (Public => False,
         As     => (User.Receptionist  => True,
                    others              => False));
      Service_Agent : constant Authentication :=
        (Public => False,
         As     => (User.Service_Agent => True,
                    others              => False));
      Administrator : constant Authentication :=
        (Public => False,
         As     => (User.Administrator => True,
                    others             => False));

      function "or" (Left, Right : in Authentication) return Authentication;
      function "or" (Left  : in Boolean;
                     Right : in Authentication) return Authentication;
   end Permission_Operations;

   package body Permission_Operations is
      function "or"  (Left, Right : in Authentication) return Authentication is
         use Model.User;
      begin
         if Left.Public or Right.Public then
            return Public;
         else
            return (Public => False,
                    As     => Left.As or Right.As);
         end if;
      end "or";

      function "or" (Left  : in Boolean;
                     Right : in Authentication) return Authentication is
      begin
         if Left then
            return Public;
         else
            return Right;
         end if;
      end "or";
   end Permission_Operations;

   function Public_User_Identification return Boolean
     renames Alice_Configuration.Public_User_Identification;

   use AWS.Status;
   use Handlers, Handlers.Authenticated_Dispatcher;
   use Permission_Operations;
begin
   Set_Default (Action => Not_Found.Callback);
   --  If no request methods has been set in the request we treat the
   --  request as failed and return a 404 Not Found.

   Set_Default (Method => OPTIONS,
                Action => CORS_Preflight.Callback);
   --  This is for CORS preflight requests.

   pragma Style_Checks ("M100"); --  Allow long lines in the routing table

   Register (GET,  "/agent",              Receptionist,           Agent.Agent'Access);
   Register (GET,  "/agent/list",         Receptionist,           Agent.Agent_List'Access);

   Register (GET,  "/call/list",          Receptionist,           Call.List'Access);
   Register (GET,  "/call/queue",         Receptionist,           Call.Queue'Access);

   Register (POST, "/call/hangup",        Receptionist,           Call.Hangup'Access);
   Register (POST, "/call/originate",     Receptionist,           Call.Originate'Access);
   Register (POST, "/call/park",          Receptionist,           Call.Park'Access);
   Register (POST, "/call/pickup",        Receptionist,           Call.Pickup'Access);
   Register (POST, "/call/transfer",      Receptionist,           Call.Transfer'Access);

   Register (GET,  "/configuration",      Receptionist,           Configuration.Callback);

   Register (GET,  "/contact",            Receptionist or Service_Agent,
                                                                  Contact.Callback);

   Register (GET,  "/debug/channel/list", Logged_In,              Debug.Channel_List'Access);
   Register (GET,  "/debug/peer/list",    Logged_In,              Debug.Channel_List'Access);

   Register (POST, "/log/critical",       Logged_In,              Log.Critical);
   Register (POST, "/log/error",          Logged_In,              Log.Error);
   Register (POST, "/log/info",           Logged_In,              Log.Info);

   Register (GET,  "/organization",       Receptionist or Service_Agent,
                                                                  Organization.Callback);
   Register (GET,  "/organization/list",  Receptionist or Service_Agent,
                                                                  Organization_List.Callback);

   Register (GET,  "/users/list",         Public_User_Identification or Administrator,
                                                                  Users.List.Callback);
   Register (GET,  "/users/log_in",       Public,                 Users.Log_In.Callback);
   Register (GET,  "/users/log_out",      Public,                 Users.Log_Out.Callback);
   Register (GET,  "/users/logged_in",    Public,                 Users.Logged_In.Callback);
   Register (GET,  "/users/logged_out",   Public,                 Users.Logged_Out.Callback);
   Register (GET,  "/users/openids",      Public_User_Identification or Administrator,
                                                                  Users.OpenIDs.Callback);
   Register (GET,  "/users/validate",     Public,                 Users.Validate.Callback);

   AWS.Net.WebSocket.Registry.Register
     (URI     => "/notifications",
      Factory => Notifications.Create'Access);

end Alice_Handlers;
