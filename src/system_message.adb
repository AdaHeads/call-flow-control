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

with Common;
with GNATCOLL.JSON;

package body System_Message is

   type Delimiter_Placement is (Fore, Aft, None);

   procedure Build_Response_Object
     (Description     : in     Unbounded_String;
      Response_Object :    out Response.Object;
      Status          : in     Unbounded_String;
      Status_Code     : in     AWS.Messages.Status_Code;
      Tail            : in     String);
   --  Add JSON content and HTTP code to Response_Object.

   function Build_String
     (S : in String;
      D : in Delimiter_Placement)
      return String;
   --  Add the " - " delimiter to S in D placement.

   procedure Logger
     (Status      : in String;
      Tail        : in String;
      Log_Trace   : in Yolk.Log.Trace_Handles);
   --  Write Status - Tail to Log_Trace.

   procedure Logger
     (Description : in String;
      Status      : in String;
      Tail        : in String;
      Log_Trace   : in Yolk.Log.Trace_Handles);
   --  Write Status - Description - Tail to Log_Trace.

   -----------------------------
   --  Build_Response_Object  --
   -----------------------------

   procedure Build_Response_Object
     (Description     : in     Unbounded_String;
      Response_Object :    out Response.Object;
      Status          : in     Unbounded_String;
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

      Response_Object.Set_Content (To_JSON_String (JSON.Write));
      Response_Object.Set_HTTP_Status_Code (Status_Code);
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

   --------------
   --  Create  --
   --------------

   function Create
     (Status : in String)
      return Critical_Log_Object
   is
      use Common;
      use Yolk.Log;
   begin
      return (Log_Trace => Critical,
              Status    => U (Status));
   end Create;

   --------------
   --  Create  --
   --------------

   function Create
     (Description : in String;
      Status      : in String;
      Status_Code : in AWS.Messages.Status_Code)
      return Critical_Response_Object
   is
      use Common;
   begin
      return (Description => U (Description),
              Status      => U (Status),
              Status_Code => Status_Code);
   end Create;

   --------------
   --  Create  --
   --------------

   function Create
     (Description : in String;
      Status      : in String;
      Status_Code : in AWS.Messages.Status_Code)
      return Critical_Log_And_Response_Object
   is
      use Common;
   begin
      return (Description => U (Description),
              Log_Trace   => Yolk.Log.Critical,
              Status      => U (Status),
              Status_Code => Status_Code);
   end Create;

   --------------
   --  Create  --
   --------------

   function Create
     (Status : in String)
      return Error_Log_Object
   is
      use Common;
      use Yolk.Log;
   begin
      return (Log_Trace => Error,
              Status    => U (Status));
   end Create;

   --------------
   --  Create  --
   --------------

   function Create
     (Description : in String;
      Status      : in String;
      Status_Code : in AWS.Messages.Status_Code)
      return Error_Response_Object
   is
      use Common;
   begin
      return (Description => U (Description),
              Status      => U (Status),
              Status_Code => Status_Code);
   end Create;

   --------------
   --  Create  --
   --------------

   function Create
     (Description : in String;
      Status      : in String;
      Status_Code : in AWS.Messages.Status_Code)
      return Error_Log_And_Response_Object
   is
      use Common;
   begin
      return (Description => U (Description),
              Log_Trace   => Yolk.Log.Error,
              Status      => U (Status),
              Status_Code => Status_Code);
   end Create;

   --------------
   --  Create  --
   --------------

   function Create
     (Status : in String)
      return Info_Log_Object
   is
      use Common;
      use Yolk.Log;
   begin
      return (Log_Trace => Info,
              Status    => U (Status));
   end Create;

   --------------
   --  Create  --
   --------------

   function Create
     (Description : in String;
      Status      : in String;
      Status_Code : in AWS.Messages.Status_Code)
      return Info_Response_Object
   is
      use Common;
   begin
      return (Description => U (Description),
              Status      => U (Status),
              Status_Code => Status_Code);
   end Create;

   --------------
   --  Create  --
   --------------

   function Create
     (Description : in String;
      Status      : in String;
      Status_Code : in AWS.Messages.Status_Code)
      return Info_Log_And_Response_Object
   is
      use Common;
   begin
      return (Description => U (Description),
              Log_Trace   => Yolk.Log.Info,
              Status      => U (Status),
              Status_Code => Status_Code);
   end Create;

   --------------
   --  Logger  --
   --------------

   procedure Logger
     (Status      : in String;
      Tail        : in String;
      Log_Trace   : in Yolk.Log.Trace_Handles)
   is
      use Yolk.Log;
   begin
      Trace (Log_Trace, Status & Build_String (Tail, Fore));
   end Logger;

   --------------
   --  Logger  --
   --------------

   procedure Logger
     (Description : in String;
      Status      : in String;
      Tail        : in String;
      Log_Trace   : in Yolk.Log.Trace_Handles)
   is
      use Yolk.Log;
   begin
      Trace (Log_Trace,
             Status
             & Build_String (Description, Fore)
             & Build_String (Tail, Fore));
   end Logger;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O : in Notice_Log_Object)
   is
   begin
      O.Notify (Message => "");
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O       : in Notice_Log_Object;
      Message : in String)
   is
   begin
      Logger (Status      => To_String (O.Status),
              Tail        => Message,
              Log_Trace   => O.Log_Trace);
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O      : in Notice_Log_Object;
      Event  : in Ada.Exceptions.Exception_Occurrence)
   is
   begin
      O.Notify (Event   => Event,
                Message => "");
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O       : in Notice_Log_Object;
      Event   : in Ada.Exceptions.Exception_Occurrence;
      Message : in String)
   is
      use Ada.Exceptions;

      E_Msg   : constant String := Exception_Message (Event);
      New_Msg : Unbounded_String := Null_Unbounded_String;
   begin
      Append (Source   => New_Msg,
              New_Item => Build_String (Message, Aft));

      Append (Source   => New_Msg,
              New_Item => Build_String (Exception_Name (Event), None));

      Append (Source   => New_Msg,
              New_Item => Build_String (E_Msg, Fore));

      O.Notify (Message => To_String (New_Msg));
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O               : in     Notice_Response_Object;
      Response_Object :    out Response.Object)
   is
   begin
      O.Notify (Message         => "",
                Response_Object => Response_Object);
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O               : in     Notice_Response_Object;
      Message         : in     String;
      Response_Object :    out Response.Object)
   is
   begin
      Build_Response_Object
        (Description     => O.Description,
         Response_Object => Response_Object,
         Status          => O.Status,
         Status_Code     => O.Status_Code,
         Tail            => Message);
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O               : in     Notice_Response_Object;
      Event           : in     Ada.Exceptions.Exception_Occurrence;
      Response_Object :    out Response.Object)
   is
   begin
      O.Notify (Event   => Event,
                Message         => "",
                Response_Object => Response_Object);
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O               : in     Notice_Response_Object;
      Event           : in     Ada.Exceptions.Exception_Occurrence;
      Message         : in     String;
      Response_Object :    out Response.Object)
   is
      use Ada.Exceptions;

      E_Msg   : constant String := Exception_Message (Event);
      New_Msg : Unbounded_String := Null_Unbounded_String;
   begin
      Append (Source   => New_Msg,
              New_Item => Build_String (Message, Aft));

      Append (Source   => New_Msg,
              New_Item => Build_String (Exception_Name (Event), None));

      Append (Source   => New_Msg,
              New_Item => Build_String (E_Msg, Fore));

      O.Notify (Message         => To_String (New_Msg),
                Response_Object => Response_Object);
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O               : in     Notice_Log_And_Response_Object;
      Response_Object :    out Response.Object)
   is
   begin
      O.Notify (Message         => "",
                Response_Object => Response_Object);
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O               : in     Notice_Log_And_Response_Object;
      Message         : in     String;
      Response_Object :    out Response.Object)
   is
   begin
      Logger (Description => To_String (O.Description),
              Status      => To_String (O.Status),
              Tail        => Message,
              Log_Trace   => O.Log_Trace);

      Build_Response_Object
        (Description     => O.Description,
         Response_Object => Response_Object,
         Status          => O.Status,
         Status_Code     => O.Status_Code,
         Tail            => Message);
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O               : in     Notice_Log_And_Response_Object;
      Event           : in     Ada.Exceptions.Exception_Occurrence;
      Response_Object :    out Response.Object)
   is
   begin
      O.Notify (Event   => Event,
                Message         => "",
                Response_Object => Response_Object);
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O               : in     Notice_Log_And_Response_Object;
      Event           : in     Ada.Exceptions.Exception_Occurrence;
      Message         : in     String;
      Response_Object :    out Response.Object)
   is
      use Ada.Exceptions;

      E_Msg   : constant String := Exception_Message (Event);
      New_Msg : Unbounded_String := Null_Unbounded_String;
   begin
      Append (Source   => New_Msg,
              New_Item => Build_String (Message, Aft));

      Append (Source   => New_Msg,
              New_Item => Build_String (Exception_Name (Event), None));

      Append (Source   => New_Msg,
              New_Item => Build_String (E_Msg, Fore));

      O.Notify (Message         => To_String (New_Msg),
                Response_Object => Response_Object);
   end Notify;

   ---------------------------------------------
   -- Notify procedures for Critical objects  --
   ---------------------------------------------

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O : in Critical_Log_Object)
   is
   begin
      Notify (Notice_Log_Object (O));
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O       : in Critical_Log_Object;
      Message : in String)
   is
   begin
      Notify (Notice_Log_Object (O), Message);
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O     : in Critical_Log_Object;
      Event : in Ada.Exceptions.Exception_Occurrence)
   is
   begin
      Notify (Notice_Log_Object (O), Event, "");
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O       : in Critical_Log_Object;
      Event   : in Ada.Exceptions.Exception_Occurrence;
      Message : in String)
   is
   begin
      Notify (Notice_Log_Object (O), Event, Message);
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O               : in     Critical_Response_Object;
      Response_Object :    out Response.Object)
   is
   begin
      Notify (Notice_Response_Object (O), Response_Object);
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O               : in     Critical_Response_Object;
      Message         : in     String;
      Response_Object :    out Response.Object)
   is
   begin
      Notify (Notice_Response_Object (O), Message, Response_Object);
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O               : in     Critical_Response_Object;
      Event           : in     Ada.Exceptions.Exception_Occurrence;
      Response_Object :    out Response.Object)
   is
   begin
      Notify (Notice_Response_Object (O), Event, Response_Object);
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O               : in     Critical_Response_Object;
      Event           : in     Ada.Exceptions.Exception_Occurrence;
      Message         : in     String;
      Response_Object :    out Response.Object)
   is
   begin
      Notify (Notice_Response_Object (O), Event, Message, Response_Object);
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O               : in     Critical_Log_And_Response_Object;
      Response_Object :    out Response.Object)
   is
   begin
      Notify (Notice_Log_And_Response_Object (O), Response_Object);
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O               : in     Critical_Log_And_Response_Object;
      Message         : in     String;
      Response_Object :    out Response.Object)
   is
   begin
      Notify (Notice_Log_And_Response_Object (O), Message, Response_Object);
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O               : in     Critical_Log_And_Response_Object;
      Event           : in     Ada.Exceptions.Exception_Occurrence;
      Response_Object :    out Response.Object)
   is
   begin
      Notify (Notice_Log_And_Response_Object (O), Event, Response_Object);
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O               : in     Critical_Log_And_Response_Object;
      Event           : in     Ada.Exceptions.Exception_Occurrence;
      Message         : in     String;
      Response_Object :    out Response.Object)
   is
   begin
      Notify
        (Notice_Log_And_Response_Object (O), Event, Message, Response_Object);
   end Notify;

   ------------------------------------------
   -- Notify procedures for Error objects  --
   ------------------------------------------

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O : in Error_Log_Object)
   is
   begin
      Notify (Notice_Log_Object (O));
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O       : in Error_Log_Object;
      Message : in String)
   is
   begin
      Notify (Notice_Log_Object (O), Message);
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O     : in Error_Log_Object;
      Event : in Ada.Exceptions.Exception_Occurrence)
   is
   begin
      Notify (Notice_Log_Object (O), Event, "");
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O       : in Error_Log_Object;
      Event   : in Ada.Exceptions.Exception_Occurrence;
      Message : in String)
   is
   begin
      Notify (Notice_Log_Object (O), Event, Message);
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O               : in     Error_Response_Object;
      Response_Object :    out Response.Object)
   is
   begin
      Notify (Notice_Response_Object (O), Response_Object);
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O               : in     Error_Response_Object;
      Message         : in     String;
      Response_Object :    out Response.Object)
   is
   begin
      Notify (Notice_Response_Object (O), Message, Response_Object);
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O               : in     Error_Response_Object;
      Event           : in     Ada.Exceptions.Exception_Occurrence;
      Response_Object :    out Response.Object)
   is
   begin
      Notify (Notice_Response_Object (O), Event, Response_Object);
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O               : in     Error_Response_Object;
      Event           : in     Ada.Exceptions.Exception_Occurrence;
      Message         : in     String;
      Response_Object :    out Response.Object)
   is
   begin
      Notify (Notice_Response_Object (O), Event, Message, Response_Object);
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O               : in     Error_Log_And_Response_Object;
      Response_Object :    out Response.Object)
   is
   begin
      Notify (Notice_Log_And_Response_Object (O), Response_Object);
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O               : in     Error_Log_And_Response_Object;
      Message         : in     String;
      Response_Object :    out Response.Object)
   is
   begin
      Notify (Notice_Log_And_Response_Object (O), Message, Response_Object);
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O               : in     Error_Log_And_Response_Object;
      Event           : in     Ada.Exceptions.Exception_Occurrence;
      Response_Object :    out Response.Object)
   is
   begin
      Notify (Notice_Log_And_Response_Object (O), Event, Response_Object);
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O               : in     Error_Log_And_Response_Object;
      Event           : in     Ada.Exceptions.Exception_Occurrence;
      Message         : in     String;
      Response_Object :    out Response.Object)
   is
   begin
      Notify
        (Notice_Log_And_Response_Object (O), Event, Message, Response_Object);
   end Notify;

   -----------------------------------------
   -- Notify procedures for Info objects  --
   -----------------------------------------

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O : in Info_Log_Object)
   is
   begin
      Notify (Notice_Log_Object (O));
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O       : in Info_Log_Object;
      Message : in String)
   is
   begin
      Notify (Notice_Log_Object (O), Message);
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O     : in Info_Log_Object;
      Event : in Ada.Exceptions.Exception_Occurrence)
   is
   begin
      Notify (Notice_Log_Object (O), Event, "");
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O       : in Info_Log_Object;
      Event   : in Ada.Exceptions.Exception_Occurrence;
      Message : in String)
   is
   begin
      Notify (Notice_Log_Object (O), Event, Message);
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O               : in     Info_Response_Object;
      Response_Object :    out Response.Object)
   is
   begin
      Notify (Notice_Response_Object (O), Response_Object);
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O               : in     Info_Response_Object;
      Message         : in     String;
      Response_Object :    out Response.Object)
   is
   begin
      Notify (Notice_Response_Object (O), Message, Response_Object);
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O               : in     Info_Response_Object;
      Event           : in     Ada.Exceptions.Exception_Occurrence;
      Response_Object :    out Response.Object)
   is
   begin
      Notify (Notice_Response_Object (O), Event, Response_Object);
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O               : in     Info_Response_Object;
      Event           : in     Ada.Exceptions.Exception_Occurrence;
      Message         : in     String;
      Response_Object :    out Response.Object)
   is
   begin
      Notify (Notice_Response_Object (O), Event, Message, Response_Object);
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O               : in     Info_Log_And_Response_Object;
      Response_Object :    out Response.Object)
   is
   begin
      Notify (Notice_Log_And_Response_Object (O), Response_Object);
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O               : in     Info_Log_And_Response_Object;
      Message         : in     String;
      Response_Object :    out Response.Object)
   is
   begin
      Notify (Notice_Log_And_Response_Object (O), Message, Response_Object);
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O               : in     Info_Log_And_Response_Object;
      Event           : in     Ada.Exceptions.Exception_Occurrence;
      Response_Object :    out Response.Object)
   is
   begin
      Notify (Notice_Log_And_Response_Object (O), Event, Response_Object);
   end Notify;

   --------------
   --  Notify  --
   --------------

   procedure Notify
     (O               : in     Info_Log_And_Response_Object;
      Event           : in     Ada.Exceptions.Exception_Occurrence;
      Message         : in     String;
      Response_Object :    out Response.Object)
   is
   begin
      Notify
        (Notice_Log_And_Response_Object (O), Event, Message, Response_Object);
   end Notify;

end System_Message;
