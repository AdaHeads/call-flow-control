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

package System_Message.Info is

   procedure Alice_Start is new Logger
     (Log_Trace => Yolk.Log.Info,
      Status    => "Alice startup");

   procedure Alice_Stop is new Logger
     (Log_Trace => Yolk.Log.Info,
      Status    => "Alice controlled shutdown. Goodbye");

   procedure Client_Info is new Log_And_Respond
     (Description => "",
      Log_Trace   => Yolk.Log.Info,
      Status      => "Client log",
      Status_Code => HTTP_Codes.No_Content);

   procedure Database_Connection_Manager_Stop is new Logger
     (Log_Trace => Yolk.Log.Info,
      Status    => "Stopping database connection maintenance task");

   procedure Notifications_WebSocket_Created is new Logger
     (Log_Trace => Yolk.Log.Info,
      Status    => "Created a /notifications WebSocket");

   procedure Notifications_WebSocket_Opened is new Logger
     (Log_Trace => Yolk.Log.Info,
      Status    => "Opened a /notifications WebSocket");

   procedure Notifications_WebSocket_Closed is new Logger
     (Log_Trace => Yolk.Log.Info,
      Status    => "Closed a /notifications WebSocket");

   procedure SIGHUP_Caught is new Logger
     (Log_Trace => Yolk.Log.Info,
      Status    => "Caught SIGHUP signal");

   procedure SIGHUP_Watcher_Start is new Logger
     (Log_Trace => Yolk.Log.Info,
      Status    => "Starting SIGHUP watcher");

   procedure SIGHUP_Watcher_Stop is new Logger
     (Log_Trace => Yolk.Log.Info,
      Status    => "Stopping SIGHUP watcher");

   procedure Jacob_Wants_To_See_This is new Logger
     (Log_Trace => Yolk.Log.Info,
      Status    => "Jacob wants to see this.");

   procedure Dial_Plan is new Logger
     (Log_Trace => Yolk.Log.Info,
      Status    => "libdialplan");

   procedure Not_Authorized is new Logger
     (Log_Trace   => Yolk.Log.Info,
      Status      => "not authorized");

   procedure OpenID_Log_In_Attempt is new Logger
     (Log_Trace   => Yolk.Log.Info,
      Status      => "OpenID log-in attempt");

end System_Message.Info;
