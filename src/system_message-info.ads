-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                            System_Message.Info                            --
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

package System_Message.Info is

   Alice_Startup : constant Info_Log_Object := Create
     (Status => "Alice startup");

   Alice_Stop : constant Info_Log_Object := Create
     (Status => "Alice controlled shutdown. Goodbye");

   Notifications_WebSocket_Created : constant Info_Log_Object := Create
     (Status => "Created a /notifications WebSocket");

   Notifications_WebSocket_Opened : constant Info_Log_Object := Create
     (Status => "Opened a /notifications WebSocket");

   Notifications_WebSocket_Closed : constant Info_Log_Object := Create
     (Status => "Closed a /notifications WebSocket");

end System_Message.Info;
