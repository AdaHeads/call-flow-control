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

with Ada.Exceptions;
with Ada.Strings.Unbounded;
with AWS.Messages;
with Response;
with Yolk.Log;

package System_Message is

   use Ada.Strings.Unbounded;

   type Object is abstract tagged limited
      record
         Status : Unbounded_String;
      end record;

   --------------------------------------------------
   --  Log_Object type, methods and derived types  --
   --------------------------------------------------

   type Log_Object is abstract limited new Object with
      record
         Log_Trace : Yolk.Log.Trace_Handles;
      end record;

   procedure Notify
     (O : in Log_Object'Class);
   --  Write O to log.

   procedure Notify
     (O       : in Log_Object'Class;
      Message : in String);
   --  Append Message To O and write to log.

   procedure Notify
     (O     : in Log_Object'Class;
      Event : in Ada.Exceptions.Exception_Occurrence);
   --  Append Event to O and write to log.

   procedure Notify
     (O       : in Log_Object'Class;
      Event   : in Ada.Exceptions.Exception_Occurrence;
      Message : in String);
   --  Append Event and Message to O and write to log.

   type Critical_Log_Object is new Log_Object with null record;
   type Error_Log_Object is new Log_Object with null record;
   type Info_Log_Object is new Log_Object with null record;

   -------------------------------------------------------
   --  Response_Object type, methods and derived types  --
   -------------------------------------------------------

   type Response_Object is abstract limited new Object with
      record
         Description : Unbounded_String;
         Status_Code : AWS.Messages.Status_Code;
      end record;

   procedure Notify
     (O               : in     Response_Object'Class;
      Response_Object :    out Response.Object);
   --  Write O to Response_Object.

   procedure Notify
     (O               : in     Response_Object'Class;
      Message         : in     String;
      Response_Object :    out Response.Object);
   --  Append Message To O and write to Response_Object.

   procedure Notify
     (O               : in     Response_Object'Class;
      Event           : in     Ada.Exceptions.Exception_Occurrence;
      Response_Object :    out Response.Object);
   --  Append Event to O and write to Response_Object.

   procedure Notify
     (O               : in     Response_Object'Class;
      Event           : in     Ada.Exceptions.Exception_Occurrence;
      Message         : in     String;
      Response_Object :    out Response.Object);
   --  Append Event and Message to O and write to Response_Object.

   type Critical_Response_Object is new Response_Object with null record;
   type Error_Response_Object is new Response_Object with null record;
   type Info_Response_Object is new Response_Object with null record;

   ---------------------------------------------------------------
   --  Log_And_Response_Object type, methods and derived types  --
   ---------------------------------------------------------------

   type Log_And_Response_Object is abstract limited new Object with
      record
         Description : Unbounded_String;
         Log_Trace   : Yolk.Log.Trace_Handles;
         Status_Code : AWS.Messages.Status_Code;
      end record;

   procedure Notify
     (O               : in     Log_And_Response_Object'Class;
      Response_Object :    out Response.Object);
   --  Write O to log and Response_Object.

   procedure Notify
     (O               : in     Log_And_Response_Object'Class;
      Message         : in     String;
      Response_Object :    out Response.Object);
   --  Append Message To O and write to log and Response_Object.

   procedure Notify
     (O               : in     Log_And_Response_Object'Class;
      Event           : in     Ada.Exceptions.Exception_Occurrence;
      Response_Object :    out Response.Object);
   --  Append Event to O and write to log and Response_Object.

   procedure Notify
     (O               : in     Log_And_Response_Object'Class;
      Event           : in     Ada.Exceptions.Exception_Occurrence;
      Message         : in     String;
      Response_Object :    out Response.Object);
   --  Append Event and Message to O and write to log and Response_Object.

   type Critical_Log_And_Response_Object is new Log_And_Response_Object with
     null record;
   type Error_Log_And_Response_Object is new Log_And_Response_Object with
     null record;
   type Info_Log_And_Response_Object is new Log_And_Response_Object with
     null record;

   function Create
     (Status : in String)
      return Critical_Log_Object;
   --  Initialize a Critical_Log_Object object.

   function Create
     (Description : in String;
      Status      : in String;
      Status_Code : in AWS.Messages.Status_Code)
      return Critical_Response_Object;
   --  Initialize a Critical_Response_Object object.

   function Create
     (Description : in String;
      Status      : in String;
      Status_Code : in AWS.Messages.Status_Code)
      return Critical_Log_And_Response_Object;
   --  Initialize a Critical_Log_And_Response_Object object.

   function Create
     (Status : in String)
      return Error_Log_Object;
   --  Initialize an Error_Log_Object object.

   function Create
     (Description : in String;
      Status      : in String;
      Status_Code : in AWS.Messages.Status_Code)
      return Error_Response_Object;
   --  Initialize an Error_Response_Object object.

   function Create
     (Description : in String;
      Status      : in String;
      Status_Code : in AWS.Messages.Status_Code)
      return Error_Log_And_Response_Object;
   --  Initialize a Error_Log_And_Response_Object object.

   function Create
     (Status : in String)
      return Info_Log_Object;
   --  Initialize an Info_Log_Object object.

   function Create
     (Description : in String;
      Status      : in String;
      Status_Code : in AWS.Messages.Status_Code)
      return Info_Response_Object;
   --  Initialize an Info_Response_Object object.

   function Create
     (Description : in String;
      Status      : in String;
      Status_Code : in AWS.Messages.Status_Code)
      return Info_Log_And_Response_Object;
   --  Initialize a Info_Log_And_Response_Object object.

end System_Message;
