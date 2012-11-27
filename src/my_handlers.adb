-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                               My_Handlers                                 --
--                                                                           --
--                                  BODY                                     --
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
with AWS.Net.WebSocket.Registry;
with Handlers.Contact;
with Handlers.Agent;
with Handlers.Call;
with Handlers.Notifications;
with Handlers.Organization;
with Handlers.Organization_List;
with My_Configuration;
with Not_Found;

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
      use My_Configuration;

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

      ------------------
      --  Dispatchers --
      ------------------

      AWS.Services.Dispatchers.URI.Register
        (Dispatcher => RH,
         URI        => Config.Get (Handler_Call_Hangup),
         Action     => Create
           (Callback => Call.Hangup'Access));

      AWS.Services.Dispatchers.URI.Register
        (Dispatcher => RH,
         URI        => Config.Get (Handler_Call_Hold),
         Action     => Create
           (Callback => Call.Hold'Access));

      AWS.Services.Dispatchers.URI.Register
        (Dispatcher => RH,
         URI        => Config.Get (Handler_Call_Pickup),
         Action     => Create
           (Callback => Call.Pickup'Access));

      AWS.Services.Dispatchers.URI.Register
        (Dispatcher => RH,
         URI        => Config.Get (Handler_Contact),
         Action     => Contact.Callback);

      AWS.Services.Dispatchers.URI.Register
        (Dispatcher => RH,
         URI        => Config.Get (Handler_Organization),
         Action     => Organization.Callback);

      AWS.Services.Dispatchers.URI.Register
        (Dispatcher => RH,
         URI        => Config.Get (Handler_Organization_List),
         Action     => Organization_List.Callback);

      AWS.Services.Dispatchers.URI.Register
        (Dispatcher => RH,
         URI        => Config.Get (Handler_Call_Queue),
         Action     => Create
           (Callback => Call.Queue'Access));

      AWS.Services.Dispatchers.URI.Register
        (Dispatcher => RH,
         URI        => Config.Get (Handler_Call_List),
         Action     => Create
           (Callback => Call.List'Access));

      AWS.Services.Dispatchers.URI.Register
        (Dispatcher => RH,
         URI        => Config.Get (Handler_Agent_List),
         Action     => Create
           (Callback => Agent.Agent_List'Access));

      AWS.Services.Dispatchers.URI.Register
        (Dispatcher => RH,
         URI        => Config.Get (Handler_Agent),
         Action     => Create
           (Callback => Agent.Agent'Access));

      ----------------------
      --  Debug handlers  --
      ----------------------

      AWS.Services.Dispatchers.URI.Register
        (Dispatcher => RH,
         URI        => Config.Get (Handler_Debug_Peer_List),
         Action     => Create
           (Callback => Handler.Debug.Peer_List'Access));

      --------------------------
      --  WebSocket handlers  --
      --------------------------

      AWS.Net.WebSocket.Registry.Register
        (URI     => Config.Get (Handler_Notifications),
         Factory => Notifications.Create'Access);

      return RH;
   end Get;

end My_Handlers;
