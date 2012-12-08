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

with AWS.Response;
with AWS.Status;

package Handlers.Call is

   Package_Name : constant String := "Handlers.Call";

--     function Bridge
--       (Request : in AWS.Status.Data)
--        return AWS.Response.Data;
--     --  Bridges two calls in the PBX

   function Originate
     (Request : in AWS.Status.Data)
      return AWS.Response.Data;
   --  Places a new call from the agent to a given extension.

   function Hangup
     (Request : in AWS.Status.Data)
      return AWS.Response.Data;
   --  End a call in progress, regardless of state

   function Park
     (Request : in AWS.Status.Data)
      return AWS.Response.Data;
   --  Put current call on hold. Return No Content if there is no call
   --  to be put on hold.

   function Pickup
     (Request : in AWS.Status.Data)
      return AWS.Response.Data;
   --  Pickup either the oldest call in the queue, or the call identified by
   --  the call_id GET parameter.

   function List
     (Request : in AWS.Status.Data)
      return AWS.Response.Data;
   --  Returns the full call list, regardless of state.

   function Queue
     (Request : in AWS.Status.Data)
      return AWS.Response.Data;
   --  Return the current list of calls queued.

end Handlers.Call;
