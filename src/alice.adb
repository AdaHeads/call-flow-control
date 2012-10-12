-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                      Copyright (C) 2012-, AdaHeads K/S                    --
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

with Ada.Directories;
with AMI.Std;
with AWS.Config;
with AWS.Server;
with AWS.Server.Log;
with AWS.Services.Dispatchers.URI;
with AWS.Session;
with My_Handlers;
with System_Message.Info;
with System_Message.Critical;
with Yolk.Configuration;
with Yolk.Log;
with Yolk.Process_Control;
with Yolk.Process_Owner;
with Yolk.Whoops;

procedure Alice is

   use My_Handlers;
   use System_Message;
   use Yolk.Configuration;
   use Yolk.Process_Control;
   use Yolk.Process_Owner;

   Alice_Version : constant String := "0.39";

   Resource_Handlers : AWS.Services.Dispatchers.URI.Handler;
   Web_Server        : AWS.Server.HTTP;
   Web_Server_Config : constant AWS.Config.Object := Get_AWS_Configuration;

   procedure Start_Server;
   --  Start Alice.

   procedure Stop_Server;
   --  Stop Alice.

   --------------------
   --  Start_Server  --
   --------------------

   procedure Start_Server
   is
      use Ada.Directories;
   begin
      AMI.Std.Connect;

      if AWS.Config.Session (Web_Server_Config)
        and then Exists (Config.Get (Session_Data_File))
      then
         AWS.Session.Load (Config.Get (Session_Data_File));
         --  If sessions are enabled and the Session_Data_File exists, then
         --  load the old session data.
      end if;

      AWS.Server.Start (Web_Server => Web_Server,
                        Dispatcher => Resource_Handlers,
                        Config     => Web_Server_Config);

      if Config.Get (AWS_Access_Log_Activate) then
         AWS.Server.Log.Start
           (Web_Server => Web_Server,
            Callback   => Yolk.Log.AWS_Access_Log_Writer'Access,
            Name       => "AWS Access Log");
      end if;

      if Config.Get (AWS_Error_Log_Activate) then
         AWS.Server.Log.Start_Error
           (Web_Server => Web_Server,
            Callback   => Yolk.Log.AWS_Error_Log_Writer'Access,
            Name       => "AWS Error Log");
         --  Start the access and error logs.
      end if;

      Notify (Info.Alice_Startup,
              "Listening on port"
              & AWS.Config.Server_Port (Web_Server_Config)'Img
              & ". Alice version "
              & Alice_Version
              & ". Yolk version "
              & Yolk.Version);
   end Start_Server;

   -------------------
   --  Stop_Server  --
   -------------------

   procedure Stop_Server
   is
   begin
      AMI.Std.Disconnect;

      if AWS.Config.Session (Web_Server_Config) then
         AWS.Session.Save (Config.Get (Session_Data_File));
         --  If sessions are enabled, then save the session data to the
         --  Session_Data_File.
      end if;

      AWS.Server.Shutdown (Web_Server);

      if AWS.Server.Log.Is_Active (Web_Server) then
         AWS.Server.Log.Stop (Web_Server);
      end if;

      if AWS.Server.Log.Is_Error_Active (Web_Server) then
         AWS.Server.Log.Stop_Error (Web_Server);
      end if;

      Notify (Info.Alice_Stop);
   end Stop_Server;
begin
   Set_User (Username => Config.Get (Yolk_User));

   Set (RH => Resource_Handlers);
   --  Populate the Resource_Handlers object.

   Set_WebSocket_Handlers;
   --  Register URI's that are WebSocket enabled.

   AWS.Server.Set_Unexpected_Exception_Handler
     (Web_Server => Web_Server,
      Handler    => Yolk.Whoops.Unexpected_Exception_Handler'Access);
   --  Yolk.Whoops.Unexpected_Exception_Handler handles, well, unexpected
   --  exceptions.

   Start_Server;

   Wait;
   --  Wait here until we get a SIGINT, SIGTERM or SIGPWR.

   Stop_Server;

exception
   when Event : others =>
      Notify (Critical.Alice_Shutdown_With_Exception, Event);
      Stop_Server;
end Alice;
