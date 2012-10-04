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

package System_Message is

   type Notification_Type is
     (Database_Connection_Error,
      GET_Parameter_Error);

   procedure Notify
     (Notification : in Notification_Type);

   procedure Notify
     (Notification : in Notification_Type;
      Event        : in Ada.Exceptions.Exception_Occurrence);

   procedure Notify
     (Notification : in Notification_Type;
      Message      : in String);

   procedure Notify
     (Notification : in Notification_Type;
      Event        : in Ada.Exceptions.Exception_Occurrence;
      Message      : in String);

end System_Message;
