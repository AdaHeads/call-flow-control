-------------------------------------------------------------------------------
--                                                                           --
--                     Copyright (C) 2014-, AdaHeads K/S                     --
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
--  with AWS.Dispatchers.Callback;
--  with AWS.MIME;
--  with AWS.Net.WebSocket.Registry.Control;
--  with AWS.Server.Log;
--  with AWS.Services.Dispatchers.URI;
--  with AWS.Session;
with Util.Configuration;
with System_Messages;
with Handlers.Not_Found;

package body Util.Server is

   type State_Type is (Not_Initialized, Initialized, Started, Stopped);

   protected State_Manager is
      procedure Set
        (Current_State :    out State_Type;
         New_State     : in     State_Type);
   private
      State : State_Type := Not_Initialized;
   end State_Manager;
   --  This makes sure that Create can only ever be called once.

   procedure Load_MIME_Types
     (MIME_File : in String := "");
   --  Load MIME_File and record every MIME type. Note that the format of this
   --  file follows the common standard format used by Apache mime.types.

   ---------------------
   --  State_Manager  --
   ---------------------

   protected body State_Manager is
      procedure Set
        (Current_State :    out State_Type;
         New_State     : in     State_Type)
      is
      begin
         Current_State := State;
         State := New_State;
      end Set;
   end State_Manager;

   --------------
   --  Create  --
   --------------

   function Create
     (Unexpected : in AWS.Exceptions.Unexpected_Exception_Handler)
      return HTTP
   is
      use Util.Configuration;

      State : State_Type;
   begin
      return WS : HTTP do
         State_Manager.Set (Current_State => State,
                            New_State     => Initialized);

         if State = Not_Initialized then
            WS.Web_Server_Config := Get_AWS_Configuration;

            WS.Handle_The_Unexpected := Unexpected;
         end if;
      end return;
   end Create;

   -----------------------
   --  Load_MIME_Types  --
   -----------------------

   procedure Load_MIME_Types
     (MIME_File : in String := "")
   is
      use Ada.Directories;
      use Util.Configuration;
      use System_Messages;

      Context : constant String := Package_Name & ".Load_MIME_Types";
   begin
      if MIME_File'Length > 0
        and then Exists (MIME_File)
      then
         AWS.MIME.Load (MIME_File);
         System_Messages.Information
           (Message => "Loaded MIME types file " & MIME_File,
            Context => Context);
      else
         System_Messages.Error
           (Message => "Cannot load MIME file " & MIME_File,
            Context => Context);
      end if;
   end Load_MIME_Types;

   -------------
   --  Start  --
   -------------

   procedure Start
     (WS          : in out HTTP;
      Dispatchers : in     AWS.Dispatchers.Handler'Class)
   is
      use Ada.Directories;
      use Util.Configuration;
      use System_Messages;

      Context : constant String := Package_Name & ".Start";

      State : State_Type;
   begin
      State_Manager.Set (Current_State => State,
                         New_State     => Started);

      if State = Initialized or State = Stopped then
         if Config.Get (Load_MIME_Types_File) then
            Load_MIME_Types (Config.Get (MIME_Types_File));
         end if;

         AWS.Server.Set_Unexpected_Exception_Handler
           (Web_Server => WS.Web_Server,
            Handler    => WS.Handle_The_Unexpected);

         if AWS.Config.Session (WS.Web_Server_Config)
           and then Exists (Config.Get (Session_Data_File))
         then
            AWS.Session.Load (Config.Get (Session_Data_File));
         end if;

         AWS.Server.Start (Web_Server => WS.Web_Server,
                           Dispatcher => Dispatchers,
                           Config     => WS.Web_Server_Config);

         if Config.Get (Start_WebSocket_Servers) then
            AWS.Net.WebSocket.Registry.Control.Start;
         end if;

         if Config.Get (AWS_Access_Log_Activate) then
            AWS.Server.Log.Start
              (Web_Server => WS.Web_Server,
               Callback   => System_Messages.Access_Log'Access,
               Name       => "AWS Access Log");
         end if;

         if Config.Get (AWS_Error_Log_Activate) then
            AWS.Server.Log.Start_Error
              (Web_Server => WS.Web_Server,
               Callback   => System_Messages.Access_Log'Access,
               Name       => "AWS Error Log");
         end if;

         System_Messages.Information
           (Message => "Server " &
              AWS.Config.Server_Name (WS.Web_Server_Config) &
              " listening on port" &
              AWS.Config.Server_Port (WS.Web_Server_Config)'Img,
            Context => Context);
      end if;
   end Start;

   ------------
   --  Stop  --
   ------------

   procedure Stop
     (WS : in out HTTP)
   is
      use AWS.Dispatchers.Callback;
      use Util.Configuration;
      use System_Messages;

      Context : constant String := Package_Name & ".Stop";

      Shutdown_Dispatcher : AWS.Services.Dispatchers.URI.Handler;
      State               : State_Type;
   begin
      State_Manager.Set (Current_State => State,
                         New_State     => Stopped);

      if State = Started then
         if AWS.Config.Session (WS.Web_Server_Config) then
            AWS.Services.Dispatchers.URI.Register_Default_Callback
              (Dispatcher => Shutdown_Dispatcher,
               Action     => Create (Handlers.Not_Found.Callback));

            AWS.Server.Set (Web_Server => WS.Web_Server,
                            Dispatcher => Shutdown_Dispatcher);
            --  We're shutting down. Replace all dispatchers with a plain
            --  not found, so no more changes can happen to the sessions
            --  between saving them to file and actually shutting down the
            --  HTTP server.

            AWS.Session.Save (Config.Get (Session_Data_File));
         end if;

         if Config.Get (Start_WebSocket_Servers) then
            AWS.Net.WebSocket.Registry.Control.Shutdown;
         end if;

         AWS.Server.Shutdown (WS.Web_Server);

         if AWS.Server.Log.Is_Active (WS.Web_Server) then
            AWS.Server.Log.Stop (WS.Web_Server);
         end if;

         if AWS.Server.Log.Is_Error_Active (WS.Web_Server) then
            AWS.Server.Log.Stop_Error (WS.Web_Server);
         end if;

         System_Messages.Information
           (Message => "Stopped " &
              AWS.Config.Server_Name (WS.Web_Server_Config),
            Context => Context);
      end if;
   end Stop;

end Util.Server;
