-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                         System_Message.Critical                           --
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

with HTTP_Codes;

package System_Message.Critical is

   Alice_Shutdown_With_Exception : constant Critical_Log_Object := Create
     (Status => "Shutting down Alice due to unhandled exception");

   Lost_Primary_Database : constant Critical_Log_Object := Create
     (Status => "Lost connection to primary database");

   Lost_Secondary_Database : constant Critical_Log_And_Response_Object
     := Create
       (Description =>
            "Lost connection to both primary and secondary database",
        Status      => "No database connection",
        Status_Code => HTTP_Codes.Server_Error);

   Unknown_User : constant Critical_Log_Object := Create
     (Status => "Cannot change user for process");

end System_Message.Critical;
