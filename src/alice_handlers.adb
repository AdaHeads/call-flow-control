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

with Handlers.Contact;
with Handlers.Agent;
with Handlers.Call;
with Handlers.Configuration;
with Handlers.Debug;
with Handlers.Notifications;
with Handlers.Organization;
with Handlers.Organization_List;
with Not_Found;

with AWS.Dispatchers.Callback;
with AWS.Net.WebSocket.Registry;
with AWS.Services.Dispatchers.URI;
with AWS.Status;

with Yolk.Static_Content;

package body Alice_Handlers is

   use Handlers;

   -----------
   --  Set  --
   -----------

--     procedure Set
--       (RH : out AWS.Services.Dispatchers.Method.Handler)
--     is
--        POST, GET : AWS.Services.Dispatchers.URI.Handler;
--     begin
--        -- 404
--        Register_Default_Callback
--          (Dispatcher => POST,
--          Action     => Create (Callback => Yolk.Not_Found.Generate'Access));
--        Register_Default_Callback
--          (Dispatcher => GET,
--          Action     => Create (Callback => Yolk.Not_Found.Generate'Access));
--
--        AWS.Services.Dispatchers.URI.Register
--          (Dispatcher => GET,
--           URI        => Config.Get (Handler_Call_Hangup),
--           Action     => Create
--             (Callback => Call.Hangup'Access));
--
--        AWS.Services.Dispatchers.URI.Register
--          (Dispatcher => POST,
--           URI        => Config.Get (Handler_Organization_List),
--           Action     => Organization_List.Callback);
--        AWS.Services.Dispatchers.URI.Register
--          (Dispatcher => GET,
--           URI        => Config.Get (Handler_Organization_List),
--           Action     => Organization_List.Callback);
--
--        -- ...
--
--        AWS.Services.Dispatchers.Method.Register (Dispatcher => RH,
--                                                  Method     => POST,
--                                                  Action     => POST)
--        AWS.Services.Dispatchers.Method.Register (Dispatcher => RH,
--                                                  Method     => GET,
--                                                  Action     => GET)
--        AWS.Services.Dispatchers.Method.Register (Dispatcher => RH,
--                                                  Method     => others,
--                                                  Action     => Not_Found)
--     end;

   function Get
     return AWS.Services.Dispatchers.Method.Handler
   is
      use AWS.Dispatchers;
      use AWS.Services;

      Method_Dispatcher   : Dispatchers.Method.Handler;
      URI_GET_Dispatcher  : Dispatchers.URI.Handler;
      URI_POST_Dispatcher : Dispatchers.URI.Handler;
   begin
      -----------------------------------------
      --  Unknown Resource (404) Dispatcher  --
      -----------------------------------------

      Method_Dispatcher.Register_Default_Callback
        (Action => URI_GET_Dispatcher);
      --  If no request methods has been set in the request we treat the
      --  request as a GET request.

      URI_GET_Dispatcher.Register_Default_Callback
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

      ----------------------
      --  GET Dispatchers --
      ----------------------

      Dispatchers.URI.Register
        (Dispatcher => URI_GET_Dispatcher,
         URI        => "/agent",
         Action     => Callback.Create
           (Callback => Agent.Agent'Access));

      Dispatchers.URI.Register
        (Dispatcher => URI_GET_Dispatcher,
         URI        => "/agent/list",
         Action     => Callback.Create
           (Callback => Agent.Agent_List'Access));

      Dispatchers.URI.Register
        (Dispatcher => URI_GET_Dispatcher,
         URI        => "/call/list",
         Action     => Callback.Create
           (Callback => Call.List'Access));

      Dispatchers.URI.Register
        (Dispatcher => URI_GET_Dispatcher,
         URI        => "/call/queue",
         Action     => Callback.Create
           (Callback => Call.Queue'Access));

      Dispatchers.URI.Register
        (Dispatcher => URI_GET_Dispatcher,
         URI        => "/configuration",
         Action     => Configuration.Callback);

      Dispatchers.URI.Register
        (Dispatcher => URI_GET_Dispatcher,
         URI        => "/contact",
         Action     => Contact.Callback);

      Dispatchers.URI.Register
        (Dispatcher => URI_GET_Dispatcher,
         URI        => "/debug/channel/list",
         Action     => Callback.Create
           (Callback => Handlers.Debug.Channel_List'Access));

      Dispatchers.URI.Register
        (Dispatcher => URI_GET_Dispatcher,
         URI        => "/debug/peer/list",
         Action     => Callback.Create
           (Callback => Handlers.Debug.Peer_List'Access));

      Dispatchers.URI.Register
        (Dispatcher => URI_GET_Dispatcher,
         URI        => "/organization",
         Action     => Organization.Callback);

      Dispatchers.URI.Register
        (Dispatcher => URI_GET_Dispatcher,
         URI        => "/organization/list",
         Action     => Organization_List.Callback);

      -----------------------
      --  POST Dispatchers --
      -----------------------

      Dispatchers.URI.Register
        (Dispatcher => URI_POST_Dispatcher,
         URI        => "/call/hangup",
         Action     => Callback.Create
           (Callback => Call.Hangup'Access));

      Dispatchers.URI.Register
        (Dispatcher => URI_POST_Dispatcher,
         URI        => "/call/park",
         Action     => Callback.Create
           (Callback => Call.Park'Access));

      Dispatchers.URI.Register
        (Dispatcher => URI_POST_Dispatcher,
         URI        => "/call/pickup",
         Action     => Callback.Create
           (Callback => Call.Pickup'Access));

      Dispatchers.URI.Register
        (Dispatcher => URI_POST_Dispatcher,
         URI        => "/call/transfer",
         Action     => Callback.Create
           (Callback => Call.Transfer'Access));

      Dispatchers.URI.Register
        (Dispatcher => URI_POST_Dispatcher,
         URI        => "/call/originate",
         Action     => Callback.Create
           (Callback => Handlers.Call.Originate'Access));

      -----------------------
      --  Method handlers  --
      -----------------------

--        AWS.Services.Dispatchers.Method.Register
--          (Dispatcher => GET,
--           Method     => AWS.Status.GET,
--           Action     => GET_URI);
--
--        AWS.Services.Dispatchers.Method.Register
--          (Dispatcher => POST,
--           Method     => AWS.Status.POST,
--           Action     => POST_URI);

      Dispatchers.Method.Register
        (Dispatcher => Method_Dispatcher,
         Method     => AWS.Status.GET,
         Action     => URI_GET_Dispatcher);

      Dispatchers.Method.Register
        (Dispatcher => Method_Dispatcher,
         Method     => AWS.Status.POST,
         Action     => URI_POST_Dispatcher);
      --        AWS.Services.Dispatchers.URI.Register (Dispatcher => RH,
--                                               Method     => others,
--                                               Action     => Not_Found);

      --------------------------
      --  WebSocket handlers  --
      --------------------------

      AWS.Net.WebSocket.Registry.Register
        (URI     => "/notifications",
         Factory => Notifications.Create'Access);

      return Method_Dispatcher;
   end Get;

end Alice_Handlers;
