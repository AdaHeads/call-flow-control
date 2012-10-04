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
with GNATCOLL.JSON;
with HTTP_Codes;
with Yolk.Log;

package body System_Message is

   use Ada.Strings.Unbounded;

   type Notice_Record is
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

   Notice_List : constant array (Notice_Type) of Notice_Record :=
                   (Database_Connection_Error  =>
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
     (Notice : in Notice_Type)
   is
   begin
      Notify (Notice  =>  Notice,
              Message => "");
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (Notice : in Notice_Type;
      Event  : in Ada.Exceptions.Exception_Occurrence)
   is
   begin
      Notify (Notice  => Notice,
              Event   => Event,
              Message => "");
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (Notice  : in Notice_Type;
      Message : in String)
   is
      use Common;
      use Yolk.Log;

      N : Notice_Record := Notice_List (Notice);
   begin
      if Message'Length > 0 then
         Append (Source   => N.Description,
                 New_Item => " - " & Message);
      end if;

      Trace (N.Log_Trace, Notice_Type'Image (Notice)
             & " - "
             & To_String (N.Status)
             & " - "
             & To_String (N.Description));
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (Notice  : in Notice_Type;
      Event   : in Ada.Exceptions.Exception_Occurrence;
      Message : in String)
   is
      use Ada.Exceptions;

      E_Msg   : constant String := Exception_Message (Event);
      New_Msg : Unbounded_String;
   begin
      if Message'Length > 0 then
         Append (Source   => New_Msg,
                 New_Item => Message & " - ");
      end if;

      Append (Source   => New_Msg,
              New_Item => Exception_Name (Event));

      if E_Msg'Length > 0 then
         Append (Source   => New_Msg,
                 New_Item => " - " & E_Msg);
      end if;

      Notify (Notice  => Notice,
              Message => To_String (New_Msg));
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (Notice          : in     Notice_Type;
      Response_Object :    out Response.Object)
   is
   begin
      Notify (Notice          => Notice,
              Message         => "",
              Response_Object => Response_Object);
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (Notice          : in     Notice_Type;
      Message         : in     String;
      Response_Object :    out Response.Object)
   is
      use AWS.Messages;
      use Common;
      use GNATCOLL.JSON;
      use Yolk.Log;

      JSON        : JSON_Value;
      N           : Notice_Record := Notice_List (Notice);
      HTTP_Status : constant String := Status_Code'Image (N.Status_Code);
   begin
      if Message /= "" then
         Append (Source   => N.Description,
                 New_Item => " - " & Message);
      end if;

      Trace (N.Log_Trace, Notice_Type'Image (Notice)
             & " - "
             & To_String (N.Status)
             & " - "
             & To_String (N.Description)
             & " - "
             & HTTP_Status);

      JSON := Create_Object;
      JSON.Set_Field (Field_Name => "description",
                      Field      => N.Description);
      JSON.Set_Field (Field_Name => "status",
                      Field      => N.Status);

      Response_Object.Set_Content (To_JSON_String (JSON.Write));
      Response_Object.Set_HTTP_Status_Code (N.Status_Code);
   end Notify;

end System_Message;
