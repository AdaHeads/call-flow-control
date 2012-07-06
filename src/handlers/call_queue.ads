-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                               Call_Queue                                  --
--                                                                           --
--                                  SPEC                                     --
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

with AWS.Status;
with AWS.Response;

package Call_Queue is

   function Get
     (Request : in AWS.Status.Data)
      return AWS.Response.Data;
   --  Return a response containing the call queue JSON, complete with all
   --  waiting calls and the length of the queue.

   function Get_Call
     (Request : in AWS.Status.Data)
      return AWS.Response.Data;
   --  Get a call JSON for the longest waiting call in the queue.

   function Get_Queue_Length
     (Request : in AWS.Status.Data)
      return AWS.Response.Data;
   --  Return a response containing a JSON with just the length of the current
   --  queue.

end Call_Queue;
