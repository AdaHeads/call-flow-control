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

with AWS.Net.WebSocket;
with AWS.Status;
with Common;
with JSON;

package Handlers.Notifications is

   function Create
     (Socket  : in AWS.Net.Socket_Access;
      Request : in AWS.Status.Data)
      return AWS.Net.WebSocket.Object'Class;
   --  Create is called whenever a new WebSocket connection is made on the
   --  /notifications resource.

   procedure Broadcast
     (JSON : in Common.JSON_String);
   --  Broadcasts JSON to every client connected to the /notifications
   --  WebSocket.

   procedure Broadcast
     (Item : in JSON.JSON_Value);
   --  Broadcasts JSON to every client connected to the /notifications
   --  WebSocket.

end Handlers.Notifications;
