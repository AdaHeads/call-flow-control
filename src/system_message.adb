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

with Ada.Strings.Unbounded;
with AWS.Messages;
with Common;
with HTTP_Codes;
with Yolk.Log;
--  with GNATCOLL.JSON;
--  with Yolk.Log;

package body System_Message is

   use Ada.Strings.Unbounded;

   type Notification_Record is
      record
         Description : Unbounded_String;
         JSON        : Common.JSON_String;
         Log_Trace   : Yolk.Log.Trace_Handles;
         Status      : Unbounded_String;
         Status_Code : AWS.Messages.Status_Code;
      end record;

   function U
     (S : in String)
      return Unbounded_String
      renames To_Unbounded_String;

   Notification_List   : constant array (Notification_Type) of
     Notification_Record :=
       (Database_Connection_Error =>
            (Description => U ("DB connection error description"),
             JSON        => Common.Null_JSON_String,
             Log_Trace   => Yolk.Log.Critical,
             Status      => U ("database connection error"),
             Status_Code => HTTP_Codes.Server_Error),
        GET_Parameter_Error        =>
          (Description  => U ("GET parameter error description"),
           JSON         => Common.Null_JSON_String,
           Log_Trace    => Yolk.Log.Error,
           Status       => U ("parameter error"),
           Status_Code  => HTTP_Codes.Bad_Request));

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (Notification : in Notification_Type)
   is
   begin
      Notify (Notification => Notification,
              Message      => "");
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (Notification : in Notification_Type;
      Event        : in Ada.Exceptions.Exception_Occurrence)
   is
   begin
      Notify (Notification => Notification,
              Event        => Event,
              Message      => "");
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (Notification : in Notification_Type;
      Message      : in String)
   is
      use Common;
      use Yolk.Log;

      N : Notification_Record := Notification_List (Notification);
   begin
      if Message /= "" then
         Append (Source   => N.Description,
                 New_Item => " - " & Message);
      end if;

      Trace (N.Log_Trace, Notification_Type'Image (Notification)
             & " - "
             & To_String (N.Status)
             & " - "
             & To_String (N.Description));
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (Notification : in Notification_Type;
      Event        : in Ada.Exceptions.Exception_Occurrence;
      Message      : in String)
   is
      use Ada.Exceptions;
      use Common;
      use Yolk.Log;

      E     : Unbounded_String := U (Exception_Name (Event));
      E_Msg : constant String := Exception_Message (Event);
      N     : Notification_Record := Notification_List (Notification);
   begin
      if Message /= "" then
         Append (Source   => N.Description,
                 New_Item => " - " & Message);
      end if;

      if E_Msg /= "" then
         Append (Source   => E,
                 New_Item => " - " & E_Msg);
      end if;

      Trace (N.Log_Trace, Notification_Type'Image (Notification)
             & " - "
             & To_String (N.Status)
             & " - "
             & To_String (N.Description)
             & " - "
             & To_String (E));
   end Notify;

end System_Message;
