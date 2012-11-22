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
with AWS.Messages;
with Response;
with Yolk.Log;

package System_Message is

   use Ada.Exceptions;

   generic
      Log_Trace : Yolk.Log.Trace_Handles;
      Status    : String;
   procedure Logger
     (Event   : in Exception_Occurrence := Null_Occurrence;
      Message : in String := "");
   --  Log Status to Log_Trace. Append Event and/or Message, if given.

   generic
      Description : String;
      Log_Trace   : Yolk.Log.Trace_Handles;
      Status      : String;
      Status_Code : AWS.Messages.Status_Code;
   procedure Log_And_Respond
     (Event           : in     Exception_Occurrence := Null_Occurrence;
      Message         : in     String := "";
      Response_Object : in out Response.Object);
   --  Log Description and Status to Log_Trace.
   --  Respond to user with Description, Status and Status_Code.
   --  Append Event and/or Message to both log and response, if given.

   generic
      Description : String;
      Status      : String;
      Status_Code : AWS.Messages.Status_Code;
   procedure Responder
     (Event           : in     Exception_Occurrence := Null_Occurrence;
      Message         : in     String := "";
      Response_Object : in out Response.Object);
   --  Respond to user with Description, Status and Status_Code. Append Event
   --  and / or Message, if given.

end System_Message;
