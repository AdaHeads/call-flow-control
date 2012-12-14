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

   procedure Alice_Shutdown_With_Exception is new Logger
     (Log_Trace => Yolk.Log.Critical,
      Status    => "Shutting down Alice due to unhandled exception");

   procedure Lost_Database_Connection is new Logger
     (Log_Trace => Yolk.Log.Critical,
      Status    => "Lost connection to database");

   procedure Response_Exception is new Log_And_Respond
     (Description => "Exception raised while trying to generate content",
      Log_Trace   => Yolk.Log.Critical,
      Status      => "server error",
      Status_Code => HTTP_Codes.Server_Error);

   procedure Unhandled_Exception is new Log_And_Respond
     (Description =>
      "This is a disaster and should be fixed by someone soon",
      Log_Trace   => Yolk.Log.Critical,
      Status      => "unhandled exception",
      Status_Code => HTTP_Codes.Server_Error);

   procedure Unknown_User is new Logger
     (Log_Trace => Yolk.Log.Critical,
      Status    => "Cannot change user for process");

end System_Message.Critical;
