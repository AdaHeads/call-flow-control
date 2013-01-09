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
with Handlers.Debug;
with Handlers.Notifications;
with Handlers.Organization;
with Handlers.Organization_List;
with Not_Found;

with AWS.Dispatchers.Callback;
with AWS.Net.WebSocket.Registry;

with Yolk.Static_Content;

package body My_Handlers is

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
     return AWS.Services.Dispatchers.URI.Handler
   is
      use AWS.Dispatchers.Callback;

      RH : AWS.Services.Dispatchers.URI.Handler;
   begin
      -----------------------------------------
      --  Unknown Resource (404) Dispatcher  --
      -----------------------------------------

      AWS.Services.Dispatchers.URI.Register_Default_Callback
        (Dispatcher => RH,
         Action     => Not_Found.Callback);
      --  This dispatcher is called if the requested resource doesn't match any
      --  of the other dispatchers. It returns a 404 to the user.

      -----------------
      --  Bob files  --
      -----------------

      Yolk.Static_Content.Set_Cache_Options;
      --  Set some basic cache options.

      AWS.Services.Dispatchers.URI.Register_Regexp
        (Dispatcher => RH,
         URI        => "/bob/*.",
         Action     => Create
           (Callback => Yolk.Static_Content.Non_Compressable'Access));
      --  If a request begins with /bob/, then look for a file matching the
      --  given URI, sans domain, postfixed the WWW_Root configuration
      --  parameter.
      --  We keep this as simple as possible. If things like compressing and
      --  serverside caching of files is necessary, consider using an actual
      --  proxy / cache server like varnish in front of Alice.

      ------------------
      --  Dispatchers --
      ------------------

      AWS.Services.Dispatchers.URI.Register
        (Dispatcher => RH,
         URI        => "/call/hangup",
         Action     => Create
           (Callback => Call.Hangup'Access));

      AWS.Services.Dispatchers.URI.Register
        (Dispatcher => RH,
         URI        => "/call/park",
         Action     => Create
           (Callback => Call.Park'Access));

      AWS.Services.Dispatchers.URI.Register
        (Dispatcher => RH,
         URI        => "/call/pickup",
         Action     => Create
           (Callback => Call.Pickup'Access));

      AWS.Services.Dispatchers.URI.Register
        (Dispatcher => RH,
         URI        => "/contact",
         Action     => Contact.Callback);

      AWS.Services.Dispatchers.URI.Register
        (Dispatcher => RH,
         URI        => "/organization",
         Action     => Organization.Callback);

      AWS.Services.Dispatchers.URI.Register
        (Dispatcher => RH,
         URI        => "/organization/list",
         Action     => Organization_List.Callback);

      AWS.Services.Dispatchers.URI.Register
        (Dispatcher => RH,
         URI        => "/call/queue",
         Action     => Create
           (Callback => Call.Queue'Access));

      AWS.Services.Dispatchers.URI.Register
        (Dispatcher => RH,
         URI        => "/call/transfer",
         Action     => Create
           (Callback => Call.Transfer'Access));

      AWS.Services.Dispatchers.URI.Register
        (Dispatcher => RH,
         URI        => "/call/list",
         Action     => Create
           (Callback => Call.List'Access));

      AWS.Services.Dispatchers.URI.Register
        (Dispatcher => RH,
         URI        => "/agent/list",
         Action     => Create
           (Callback => Agent.Agent_List'Access));

      AWS.Services.Dispatchers.URI.Register
        (Dispatcher => RH,
         URI        => "/agent",
         Action     => Create
           (Callback => Agent.Agent'Access));

      AWS.Services.Dispatchers.URI.Register
        (Dispatcher => RH,
         URI        => "/call/originate",
         Action     => Create
           (Callback => Handlers.Call.Originate'Access));

      ----------------------
      --  Debug handlers  --
      ----------------------

      AWS.Services.Dispatchers.URI.Register
        (Dispatcher => RH,
         URI        => "/debug/peer/list",
         Action     => Create
           (Callback => Handlers.Debug.Peer_List'Access));

      AWS.Services.Dispatchers.URI.Register
        (Dispatcher => RH,
         URI        => "/debug/channel/list",
         Action     => Create
           (Callback => Handlers.Debug.Channel_List'Access));

      --------------------------
      --  WebSocket handlers  --
      --------------------------

      AWS.Net.WebSocket.Registry.Register
        (URI     => "/notifications",
         Factory => Notifications.Create'Access);

      return RH;
   end Get;

end My_Handlers;
