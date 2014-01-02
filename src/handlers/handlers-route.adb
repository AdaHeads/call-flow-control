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

with Handlers.Call,
     Handlers.Configuration,
     Handlers.CORS_Preflight,
     Handlers.Debug,
     Handlers.Not_Found,
     Handlers.Notifications,
     Handlers.User.List;

package body Handlers.Route is

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

   -----------------------------
   --  Permission_Operations  --
   -----------------------------

   package Permission_Operations is
      use Handlers.Authenticated_Dispatcher, Model;

      Public        : constant Authentication := (Public => True);
      Logged_In     : constant Authentication := (Public => False,
                                                  As     => (others => True));
      Receptionist  : constant Authentication :=
        (Public => False,
         As     => (Model.User.Receptionist  => True,
                    others              => False));
      Service_Agent : constant Authentication :=
        (Public => False,
         As     => (Model.User.Service_Agent => True,
                    others              => False));
      Administrator : constant Authentication :=
        (Public => False,
         As     => (Model.User.Administrator => True,
                    others             => False));

      function "or" (Left, Right : in Authentication) return Authentication;
      function "or" (Left  : in Boolean;
                     Right : in Authentication) return Authentication;
   end Permission_Operations;

   -----------------------------
   --  Permission_Operations  --
   -----------------------------

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
   use Handlers.Authenticated_Dispatcher;
   use Permission_Operations;
begin
   Set_Default (Action => Not_Found.Callback);
   --  If no request methods has been set in the request we treat the
   --  request as failed and return a 404 Not Found.

   Set_Default (Method => OPTIONS,
                Action => CORS_Preflight.Callback);
   --  This is for CORS preflight requests.

   pragma Style_Checks ("M100"); --  Allow long lines in the routing table

   --  Call control and information handlers.
   Register (GET,  "/call/list",          Receptionist,           Call.List'Access);
   Register (GET,  "/call/queue",         Receptionist,           Call.Queue'Access);
   Register (POST, "/call/hangup",        Receptionist,           Call.Hangup'Access);
   Register (POST, "/call/originate",     Receptionist,           Call.Originate'Access);
   Register (POST, "/call/park",          Receptionist,           Call.Park'Access);
   Register (POST, "/call/pickup",        Receptionist,           Call.Pickup'Access);
   Register (POST, "/call/transfer",      Receptionist,           Call.Transfer'Access);

   --  Configuration handler. Only used for basic information
   --  prior to logging in.
   Register (GET,  "/configuration",      Public,           Configuration.Callback);

   --  User related handlers.
   Register (GET,  "/user/list",         Administrator,
             User.List.Callback);
   Register (GET,  "/user",              Receptionist,                 User.Profile'Access);

   --  Debug handles, disable when in production.
   Register (GET,  "/debug/channel/list", Public, Debug.Channel_List'Access);
   Register (GET,  "/debug/peer/list",    Public, Debug.Peer_List'Access);
   Register (GET,  "/debug/token/dummy_list",    Public, Debug.Dummy_Tokens'Access);

   --  Our notification socket for asynchonous event sent to the clients.
   AWS.Net.WebSocket.Registry.Register
     (URI     => "/notifications",
      Factory => Notifications.Create'Access);

end Handlers.Route;
