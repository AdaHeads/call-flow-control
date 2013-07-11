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

with CORS_Preflight;
with Handlers.Contact;
with Handlers.Agent;
with Handlers.Call;
with Handlers.Configuration;
with Handlers.Debug;
with Handlers.Log;
with Handlers.Notifications;
with Handlers.Organization;
with Handlers.Organization_List;
with Handlers.Users.List;
with Not_Found;

with AWS.Dispatchers.Callback;
with AWS.Net.WebSocket.Registry;
with AWS.Services.Dispatchers.URI;
with AWS.Status;

with Yolk.Static_Content;

package body Alice_Handlers is

   procedure Set_GET_URI_Dispatchers
     (Dispatcher : out AWS.Services.Dispatchers.URI.Handler);
   --  Add all URI dispatchers for GET requests.

   procedure Set_POST_URI_Dispatchers
     (Dispatcher : in out AWS.Services.Dispatchers.URI.Handler);
   --  Add all URI dispatchers for POST requests.

   -----------
   --  Get  --
   -----------

   function Get
     return AWS.Services.Dispatchers.Method.Handler
   is
      use AWS.Dispatchers;
      use AWS.Services;
      use Handlers;

      Method_Dispatcher   : Dispatchers.Method.Handler;
      URI_GET_Dispatcher  : Dispatchers.URI.Handler;
      URI_POST_Dispatcher : Dispatchers.URI.Handler;
   begin
      -----------------------------------------
      --  Unknown Resource (404) Dispatcher  --
      -----------------------------------------

      Method_Dispatcher.Register_Default_Callback
        (Action => Not_Found.Callback);
      --  If no request methods has been set in the request we treat the
      --  request as failed and return a 404 Not Found.

      URI_GET_Dispatcher.Register_Default_Callback
        (Action => Not_Found.Callback);
      --  Returns a 404 to the user if no callback is found for the requested
      --  resource.

      URI_POST_Dispatcher.Register_Default_Callback
        (Action => Not_Found.Callback);
      --  Returns a 404 to the user if no callback is found for the requested
      --  resource.

      -----------------
      --  Bob files  --
      -----------------

      Yolk.Static_Content.Set_Cache_Options;
      --  Set some basic cache options for the static content.

      URI_GET_Dispatcher.Register_Regexp
        (URI    => "/bob/*.",
         Action => Callback.Create
           (Callback => Yolk.Static_Content.Non_Compressable'Access));
      --  If a request begins with /bob/, then look for a file matching the
      --  given URI, sans domain, postfixed the WWW_Root configuration
      --  parameter.
      --  We keep this as simple as possible. If things like compressing and
      --  serverside caching of files is necessary, consider using an actual
      --  proxy / cache server like varnish in front of Alice.

      ---------------------------
      --  Set URI dispatchers  --
      ---------------------------

      Set_GET_URI_Dispatchers (Dispatcher => URI_GET_Dispatcher);
      Set_POST_URI_Dispatchers (Dispatcher => URI_POST_Dispatcher);

      -----------------------
      --  Method handlers  --
      -----------------------

      Method_Dispatcher.Register
        (Method => AWS.Status.GET,
         Action => URI_GET_Dispatcher);
      --  Add all the GET URI's to the GET method dispatcher.

      Method_Dispatcher.Register
        (Method => AWS.Status.POST,
         Action => URI_POST_Dispatcher);
      --  Add all the POST URI's to the POST method dispatcher.

      Method_Dispatcher.Register
        (Method => AWS.Status.OPTIONS,
         Action => CORS_Preflight.Callback);
      --  This is for CORS preflight requests.

      --------------------------
      --  WebSocket handlers  --
      --------------------------

      AWS.Net.WebSocket.Registry.Register
        (URI     => "/notifications",
         Factory => Notifications.Create'Access);

      return Method_Dispatcher;
   end Get;

   -------------------------------
   --  Set_GET_URI_Dispatchers  --
   -------------------------------

   procedure Set_GET_URI_Dispatchers
     (Dispatcher : out AWS.Services.Dispatchers.URI.Handler)
   is
      use AWS.Dispatchers;
      use AWS.Services;
      use Handlers;
   begin
      Dispatcher.Register
        (URI    => "/agent",
         Action => Callback.Create
           (Callback => Agent.Agent'Access));

      Dispatcher.Register
        (URI    => "/agent/list",
         Action => Callback.Create
           (Callback => Agent.Agent_List'Access));

      Dispatcher.Register
        (URI    => "/call/list",
         Action => Callback.Create
           (Callback => Call.List'Access));

      Dispatcher.Register
        (URI    => "/call/queue",
         Action => Callback.Create
           (Callback => Call.Queue'Access));

      Dispatcher.Register
        (URI    => "/configuration",
         Action => Configuration.Callback);

      Dispatcher.Register
        (URI    => "/contact",
         Action => Contact.Callback);

      Dispatcher.Register
        (URI    => "/debug/channel/list",
         Action => Callback.Create
           (Callback => Handlers.Debug.Channel_List'Access));

      Dispatcher.Register
        (URI    => "/debug/peer/list",
         Action => Callback.Create
           (Callback => Handlers.Debug.Peer_List'Access));

      Dispatcher.Register
        (URI    => "/organization",
         Action => Organization.Callback);

      Dispatcher.Register
        (URI    => "/organization/list",
         Action => Organization_List.Callback);

      Dispatcher.Register
        (URI    => "/users/list",
         Action => Users.List.Callback);
   end Set_GET_URI_Dispatchers;

   --------------------------------
   --  Set_POST_URI_Dispatchers  --
   --------------------------------

   procedure Set_POST_URI_Dispatchers
     (Dispatcher : in out AWS.Services.Dispatchers.URI.Handler)
   is
      use AWS.Dispatchers;
      use AWS.Services;
      use Handlers;
   begin
      Dispatcher.Register
        (URI    => "/call/hangup",
         Action => Callback.Create
           (Callback => Call.Hangup'Access));

      Dispatcher.Register
        (URI    => "/call/park",
         Action => Callback.Create
           (Callback => Call.Park'Access));

      Dispatcher.Register
        (URI    => "/call/pickup",
         Action => Callback.Create
           (Callback => Call.Pickup'Access));

      Dispatcher.Register
        (URI    => "/call/transfer",
         Action => Callback.Create
           (Callback => Call.Transfer'Access));

      Dispatcher.Register
        (URI    => "/call/originate",
         Action => Callback.Create
           (Callback => Handlers.Call.Originate'Access));

      Dispatcher.Register
        (URI    => "/log/critical",
         Action => Log.Callback_Critical);

      Dispatcher.Register
        (URI    => "/log/error",
         Action => Log.Callback_Error);

      Dispatcher.Register
        (URI    => "/log/info",
         Action => Log.Callback_Info);
   end Set_POST_URI_Dispatchers;

end Alice_Handlers;
