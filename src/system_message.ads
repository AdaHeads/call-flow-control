-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                              System_Message                               --
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

--  with Ada.Exceptions;
with Ada.Strings.Unbounded;
with AWS.Messages;
with Common;
with Yolk.Log;

package System_Message is

   type Notification_Type is (Database_Error, GET_Parameter_Error);

   type Notification_Object is tagged private;

   function JSON
     (O : in Notification_Object)
      return Common.JSON_String;
   --  TODO: Write comment.

   function Notify
     (Notification : in Notification_Type;
      Message      : in String)
      return Notification_Object;

   function Status_Code
     (Notification : in Notification_Object)
      return AWS.Messages.Status_Code;
   --  TODO: Write comment.

private

   use Ada.Strings.Unbounded;

   type Notification_Object is tagged
      record
         Description : Unbounded_String;
         JSON        : Common.JSON_String;
         Log_Trace   : Yolk.Log.Trace_Handles;
         Status      : Unbounded_String;
         Status_Code : AWS.Messages.Status_Code;
      end record;

end System_Message;
