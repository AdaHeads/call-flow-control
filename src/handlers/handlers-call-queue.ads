-------------------------------------------------------------------------------
--                                                                           --
--                     Copyright (C) 2014-, AdaHeads K/S                     --
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

--  https://github.com/AdaHeads/Call-Flow-Control/wiki/Protocol-Call-Queue

--  Reponse handler for listing all queued calls visible to the agent.
--
--  Parameters: None
--  Returns: A JSON response with the call list embedded.

with Black.Response,
     Black.Request;

package Handlers.Call.Queue is
   Package_Name : constant String := "Handlers.Call.Queue";

   procedure Handle (Stream  : in     GNAT.Sockets.Stream_Access;
                     Request : in     Black.Request.Instance);
end Handlers.Call.Queue;
