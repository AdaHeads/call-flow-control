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

with HTTP_Codes;

package System_Message.Critical is

   procedure Alice_Shutdown_With_Exception is new Logger
     (Log_Trace => Yolk.Log.Critical,
      Status    => "Shutting down Alice due to unhandled exception");

   procedure Client_Critical is new Log_And_Respond
     (Description => "",
      Log_Trace   => Yolk.Log.Critical,
      Status      => "Client log",
      Status_Code => HTTP_Codes.No_Content);

   procedure Connection_Maintenance_Error is new Logger
     (Log_Trace => Yolk.Log.Critical,
      Status    => "Storage.Connections.Maintenance task terminated by an" &
        " unhandled exception");

   procedure Get_Storage_Connection_Error is new Logger
     (Log_Trace => Yolk.Log.Critical,
      Status    => "Database connection not available");

   procedure Lost_Database_Connection is new Logger
     (Log_Trace => Yolk.Log.Critical,
      Status    => "Lost connection to database");

   procedure No_Calendar_Database is new Logger
     (Log_Trace => Yolk.Log.Critical,
      Status    => "Calendar database has not been implemented yet.");

   procedure Please_Refill_Calendar_Database is new Logger
     (Log_Trace => Yolk.Log.Critical,
      Status    => "No events further into the future than one month.");

   procedure Response_Exception is new Log_And_Respond
     (Description => "Exception raised while trying to generate content",
      Log_Trace   => Yolk.Log.Critical,
      Status      => "server error",
      Status_Code => HTTP_Codes.Server_Error);

   procedure SIGHUP_Handler_Exception is new Logger
     (Log_Trace => Yolk.Log.Critical,
      Status    => "Error in SIGHUP handler");

   procedure SIGHUP_Register_Handler is new Logger
     (Log_Trace => Yolk.Log.Critical,
      Status    => "Too many SIGHUP handlers registered");

   procedure Unhandled_Exception is new Log_And_Respond
     (Description =>
      "This is a disaster and should be fixed by someone soon",
      Log_Trace   => Yolk.Log.Critical,
      Status      => "unhandled exception",
      Status_Code => HTTP_Codes.Server_Error);

   procedure Unknown_User is new Logger
     (Log_Trace => Yolk.Log.Critical,
      Status    => "Cannot change user for process");

   procedure Dial_Plan is new Logger
     (Log_Trace => Yolk.Log.Critical,
      Status    => "libdialplan");

   procedure Configuration_Error is new Logger
     (Log_Trace => Yolk.Log.Critical,
      Status    => "Configuration error");

   procedure Unrecoverable_PBX_Error is new Logger
     (Log_Trace => Yolk.Log.Critical,
      Status    => "PBX module failure!");

   procedure Running_In_Unsafe_Mode is new Logger
     (Log_Trace => Yolk.Log.Critical,
      Status    => "Running in unsafe mode.  Anybody can access Alice.");

end System_Message.Critical;
