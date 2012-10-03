-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                              System_Message                               --
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

with GNATCOLL.JSON;
with HTTP_Codes;
--  with Yolk.Log;

package body System_Message is

   use HTTP_Codes;

   function U
     (S : in String)
      return Unbounded_String
      renames To_Unbounded_String;

   function Build_System_Message_Record
     (Description : in String;
      Log_Trace   : in Yolk.Log.Trace_Handles;
      Status      : in String;
      Status_Code : in AWS.Messages.Status_Code)
      return Notification_Object;
   --  TODO: Write comment.

   -----------------------------------
   --  Build_System_Message_Record  --
   -----------------------------------

   function Build_System_Message_Record
     (Description     : in String;
      Log_Trace       : in Yolk.Log.Trace_Handles;
      Status          : in String;
      Status_Code     : in AWS.Messages.Status_Code)
      return Notification_Object
   is
      use Common;
      use GNATCOLL.JSON;
      --  use Yolk.Log;
   begin
      return (Description => U (Description),
              JSON        => Null_JSON_String,
              Log_Trace   => Log_Trace,
              Status      => U (Status),
              Status_Code => Status_Code);
   end Build_System_Message_Record;

   Notification_List : array (Notification_Type) of Notification_Object :=
                         (Database_Error      => Build_System_Message_Record
                            (Description     => "",
                             Log_Trace       => Yolk.Log.Error,
                             Status          => "database error",
                             Status_Code     => Server_Error),
                          GET_Parameter_Error => Build_System_Message_Record
                            (Description     => "",
                             Log_Trace       => Yolk.Log.Error,
                             Status          => "parameter error",
                             Status_Code     => Bad_Request));

   ------------
   --  JSON  --
   ------------

   function JSON
     (O : in Notification_Object)
      return Common.JSON_String
   is
   begin
      return O.JSON;
   end JSON;

   --------------
   --  Notify  --
   --------------

   function Notify
     (Notification : in Notification_Type;
      Message      : in String)
      return Notification_Object
   is
      pragma Unreferenced (Message);
   begin
      return Notification_List (Notification);
   end Notify;

   -------------------
   --  Status_Code  --
   -------------------

   function Status_Code
     (Notification : in Notification_Object)
      return AWS.Messages.Status_Code
   is
   begin
      return Notification.Status_Code;
   end Status_Code;

end System_Message;
