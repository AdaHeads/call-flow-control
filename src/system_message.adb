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

with Ada.Exceptions.Is_Null_Occurrence;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Common;
with HTTP_Codes;

with GNATCOLL.JSON;

package body System_Message is

   type Delimiter_Placement is (Fore, Aft, None);

   procedure Build_Response_Object
     (Description     : in     String;
      Response_Object :    out Response.Object;
      Status          : in     String;
      Status_Code     : in     AWS.Messages.Status_Code;
      Tail            : in     String);
   --  Add JSON content and HTTP code to Response_Object.

   function Build_String
     (S : in String;
      D : in Delimiter_Placement)
      return String;
   --  Add the " - " delimiter to S in D placement.

   function Construct_Message
     (Event   : in Exception_Occurrence;
      Message : in String)
      return String;
   --  Build a single message from the Event exception message and Message.

   procedure Write_To_Log
     (Status    : in String;
      Tail      : in String;
      Log_Trace : in Yolk.Log.Trace_Handles);
   --  Write Status - Tail to Log_Trace.

   procedure Write_To_Log
     (Description : in String;
      Status      : in String;
      Tail        : in String;
      Log_Trace   : in Yolk.Log.Trace_Handles);
   --  Write Status - Description - Tail to Log_Trace.

   -----------------------------
   --  Build_Response_Object  --
   -----------------------------

   procedure Build_Response_Object
     (Description     : in     String;
      Response_Object :    out Response.Object;
      Status          : in     String;
      Status_Code     : in     AWS.Messages.Status_Code;
      Tail            : in     String)
   is
      use Common;
      use GNATCOLL.JSON;

      JSON : JSON_Value;
   begin
      JSON := Create_Object;
      JSON.Set_Field
        (Field_Name => "description",
         Field      => Description & Build_String (Tail, Fore));
      JSON.Set_Field (Field_Name => "status",
                      Field      => Status);

      Response_Object.HTTP_Status_Code (Status_Code);
      Response_Object.Content (To_JSON_String (JSON));
   end Build_Response_Object;

   --------------------
   --  Build_String  --
   --------------------

   function Build_String
     (S : in String;
      D : in Delimiter_Placement)
      return String
   is
   begin
      if S'Length > 0 then
         case D is
            when Fore =>
               return " - " & S;
            when Aft =>
               return S & " - ";
            when None =>
               return S;
         end case;
      end if;

      return S;
   end Build_String;

   -------------------------
   --  Construct_Message  --
   -------------------------

   function Construct_Message
     (Event   : in Exception_Occurrence;
      Message : in String)
      return String
   is
      use Ada.Strings.Unbounded;

      New_Msg : Unbounded_String := Null_Unbounded_String;
   begin
      if Message /= "" then
         Append (Source   => New_Msg,
                 New_Item => Build_String (Message, None));
      end if;

      if not Ada.Exceptions.Is_Null_Occurrence (Event) then
         Append (Source   => New_Msg,
                 New_Item => Build_String (Exception_Name (Event), Fore));

         Append (Source   => New_Msg,
                 New_Item => Build_String (Exception_Message (Event), Fore));
      end if;

      return To_String (New_Msg);
   end Construct_Message;

   -----------------------
   --  Log_And_Respond  --
   -----------------------

   procedure Log_And_Respond
     (Event           : in     Exception_Occurrence := Null_Occurrence;
      Message         : in     String := "";
      Response_Object : in out Response.Object)
   is
      use AWS.Messages;
      use Common;

      New_Msg : constant String := Construct_Message (Event, Message);
   begin
      Write_To_Log (Description => Description,
                    Status      => Status,
                    Tail        => New_Msg,
                    Log_Trace   => Log_Trace);

      if Status_Code = HTTP_Codes.No_Content then
         Response_Object.HTTP_Status_Code (Status_Code);
         Response_Object.Content (Null_JSON_String);
      else
         Build_Response_Object
           (Description     => Description,
            Response_Object => Response_Object,
            Status          => Status,
            Status_Code     => Status_Code,
            Tail            => New_Msg);
      end if;
   end Log_And_Respond;

   --------------
   --  Logger  --
   --------------

   procedure Logger
     (Event   : in Exception_Occurrence := Null_Occurrence;
      Message : in String := "")
   is
      New_Msg : constant String := Construct_Message (Event, Message);
   begin
      Write_To_Log (Status    => Status,
                    Tail      => New_Msg,
                    Log_Trace => Log_Trace);
   end Logger;

   -----------------
   --  Responder  --
   -----------------

   procedure Responder
     (Event           : in     Exception_Occurrence := Null_Occurrence;
      Message         : in     String := "";
      Response_Object : in out Response.Object)
   is
      use AWS.Messages;
      use Common;

      New_Msg : constant String := Construct_Message (Event, Message);
   begin
      if Status_Code = HTTP_Codes.No_Content then
         Response_Object.HTTP_Status_Code (Status_Code);
         Response_Object.Content (Null_JSON_String);
      else
         Build_Response_Object
           (Description     => Description,
            Response_Object => Response_Object,
            Status          => Status,
            Status_Code     => Status_Code,
            Tail            => New_Msg);
      end if;
   end Responder;

   --------------------
   --  Write_To_Log  --
   --------------------

   procedure Write_To_Log
     (Status    : in String;
      Tail      : in String;
      Log_Trace : in Yolk.Log.Trace_Handles)
   is
      use Yolk.Log;
   begin
      Ada.Text_IO.Put_Line
        (Log_Trace'Img & ": " & Status & Build_String (Tail, Fore));
      Trace (Log_Trace, Status & Build_String (Tail, Fore));
   end Write_To_Log;

   --------------------
   --  Write_To_Log  --
   --------------------

   procedure Write_To_Log
     (Description : in String;
      Status      : in String;
      Tail        : in String;
      Log_Trace   : in Yolk.Log.Trace_Handles)
   is
      use Yolk.Log;
   begin
      Ada.Text_IO.Put_Line (Log_Trace'Img & ": " & Status
             & Build_String (Description, Fore)
             & Build_String (Tail, Fore));
      Trace (Log_Trace,
             Status
             & Build_String (Description, Fore)
             & Build_String (Tail, Fore));
   end Write_To_Log;

end System_Message;
