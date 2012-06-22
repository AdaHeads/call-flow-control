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

with Common;
with AWS.Messages;

package Call_Queue is

   function Get return Common.JSON_String;
   --  Return a JSON String containing the length of the queue and all the
   --  calls waiting in the queue.

   procedure Get_Call
     (Id          : in     String;
      Status_Code :    out AWS.Messages.Status_Code;
      Value       :    out Common.JSON_String);
   --  If Id exists, Value contains the data for the call with Id and
   --  Status_Code is 200.
   --  If Id does not exist, Value is an empty JSON string {} and Status_Code
   --  is 404.
   --  If Id is empty and there are calls in the queue, Value contains the
   --  data for the oldest call and Status_Code is 200.
   --  If Id is empty and the queue is empty, Value contains an empty JSON
   --  String {} and Status_Code is 404.
   --  When a call is found and returned, it is also deleted from the queue.

   function Length return Common.JSON_String;
   --  Return a JSON String containing simply the length of the queue.

end Call_Queue;
