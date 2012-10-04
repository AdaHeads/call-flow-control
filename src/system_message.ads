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
with Response;

package System_Message is

   type Notice_Type is
     (Database_Connection_Error,
      GET_Parameter_Error);

   procedure Notify
     (Notice : in Notice_Type);
   --  Write Notice to log.

   procedure Notify
     (Notice : in Notice_Type;
      Event  : in Ada.Exceptions.Exception_Occurrence);
   --  Write Notice and Event to log.

   procedure Notify
     (Notice  : in Notice_Type;
      Message : in String);
   --  Write Notice and Message to log.

   procedure Notify
     (Notice  : in Notice_Type;
      Event   : in Ada.Exceptions.Exception_Occurrence;
      Message : in String);
   --  Write Notice, Event and Message to log.

   procedure Notify
     (Notice          : in     Notice_Type;
      Response_Object :    out Response.Object);
   --  Write Notice to log, and set HTTP status code and JSON content in
   --  Response_Object according to the given Notice.

   procedure Notify
     (Notice          : in     Notice_Type;
      Event           : in     Ada.Exceptions.Exception_Occurrence;
      Response_Object :    out Response.Object);
   --  Write Notice and Event to log, and set HTTP status code and JSON content
   --  in Response_Object according to the given Notice.

   procedure Notify
     (Notice          : in     Notice_Type;
      Message         : in     String;
      Response_Object :    out Response.Object);
   --  Write Notice and Message to log, and set HTTP status code and JSON
   --  content in Response_Object according to the given Notice.

   procedure Notify
     (Notice          : in     Notice_Type;
      Event           : in     Ada.Exceptions.Exception_Occurrence;
      Message         : in     String;
      Response_Object :    out Response.Object);
   --  Write Notice, Event and Message to log, and set HTTP status code and
   --  JSON content in Response_Object according to the given Notice.

end System_Message;
