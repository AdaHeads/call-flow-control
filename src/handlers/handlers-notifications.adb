-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                           Handlers.Notifications                          --
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

with AWS.Net.WebSocket.Registry;
with System_Message.Info;

package body Handlers.Notifications is

   type Object is new AWS.Net.WebSocket.Object with null record;

   overriding procedure On_Close
     (Socket  : in out Object;
      Message : in     String);
   --  Is called when a websocket connection is closed.

   overriding procedure On_Open
     (Socket  : in out Object;
      Message : in     String);
   --  Is called when a websocket connection is opened.

   Recipients : constant AWS.Net.WebSocket.Registry.Recipient :=
                  AWS.Net.WebSocket.Registry.Create (URI => "/notifications");
   --  Targets all clients (any Origin) whose URI is /notifications. Basically
   --  the /notifications websocket broadcasts to every connected client.

   -----------------
   --  Broadcast  --
   -----------------

   procedure Broadcast
     (JSON : in Common.JSON_String)
   is
      use Common;
   begin
      AWS.Net.WebSocket.Registry.Send
        (To      => Recipients,
         Message => To_String (JSON));
   end Broadcast;

   --------------
   --  Create  --
   --------------

   function Create
     (Socket  : AWS.Net.Socket_Access;
      Request : AWS.Status.Data)
      return AWS.Net.WebSocket.Object'Class
   is
      use System_Message;
   begin
      Info.Notifications_WebSocket_Created.Notify;

      return Object'(AWS.Net.WebSocket.Object
                     (AWS.Net.WebSocket.Create (Socket, Request))
                     with null record);
   end Create;

   ----------------
   --  On_Close  --
   ----------------

   overriding procedure On_Close
     (Socket  : in out Object;
      Message : in     String)
   is
      pragma Unreferenced (Socket);
      pragma Unreferenced (Message);

      use System_Message;
   begin
      Info.Notifications_WebSocket_Closed.Notify;
   end On_Close;

   ---------------
   --  On_Open  --
   ---------------

   overriding procedure On_Open
     (Socket  : in out Object;
      Message : in     String)
   is
      pragma Unreferenced (Socket);
      pragma Unreferenced (Message);

      use System_Message;
   begin
      Info.Notifications_WebSocket_Opened.Notify;
   end On_Open;

end Handlers.Notifications;
