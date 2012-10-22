-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                 AMI.Std                                   --
--                                                                           --
--                                  BODY                                     --
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

with Ada.Calendar;
with AMI.Action;
with AMI.Event;
with AWS.Net.Std;
with AWS.Net.Buffered;
with My_Configuration;
with Yolk.Log;

package body AMI.Std is

   use My_Configuration;

   type AMI_Connection_Type is (Action, Event);

   type Callback is access procedure (Socket   : in AWS.Net.Std.Socket_Type;
                                      Username : in String;
                                      Secret   : in String);

   task type AMI_Socket
     (AMI_Type     : AMI_Connection_Type;
      AMI_Callback : Callback)
   is
      entry Start;
      --  Dummy entry call, to make it visible that we are starting AMI.
   end AMI_Socket;

   Host_Port         : constant String := Config.Get (PBX_Host) &
     ":" & Config.Get (PBX_Port);
   Socket_List       : array (AMI_Connection_Type) of AWS.Net.Std.Socket_Type;
   Shutdown          : Boolean := False;

   function AMI_Secret
     (AMI_Type : in AMI_Connection_Type)
      return String
   with inline;
   --  Returns AMI password for AMI_Type.

   function AMI_Username
     (AMI_Type : in AMI_Connection_Type)
      return String
   with inline;
   --  Returns AMI username for AMI_Type.

   ------------------
   --  AMI_Secret  --
   ------------------

   function AMI_Secret (AMI_Type : in AMI_Connection_Type) return String
   is
   begin
      case AMI_Type is
         when Action =>
            return Config.Get (PBX_Action_Secret);
         when Event =>
            return Config.Get (PBX_Event_Secret);
      end case;
   end AMI_Secret;

   --------------------
   --  AMI_Username  --
   --------------------

   function AMI_Username (AMI_Type : in AMI_Connection_Type) return String
   is
   begin
      case AMI_Type is
         when Action =>
            return Config.Get (PBX_Action_User);
         when Event =>
            return Config.Get (PBX_Event_User);
      end case;
   end AMI_Username;

   ------------------
   --  AMI_Socket  --
   ------------------

   task body AMI_Socket
   is
      use Yolk.Log;

      Reconnect_Delay : constant Duration := 0.5;
      Host            : constant String   := Config.Get (PBX_Host);
      Port            : constant Positive := Config.Get (PBX_Port);
      Socket_Event    : AWS.Net.Event_Set;
   begin
      accept Start;

      Socket_Connection :
      loop
         declare
            use Ada.Calendar;

            Wait_Until : constant Time := Clock + 3.0;
         begin
            AWS.Net.Std.Connect (Socket => Socket_List (AMI_Type),
                                 Host   => Host,
                                 Port   => Port,
                                 Wait   => False);

            Wait_For_Connect :
            loop
               Socket_Event := AWS.Net.Check
                 (Socket  => Socket_List (AMI_Type),
                  Events  => (AWS.Net.Input => True, AWS.Net.Output => True));

               exit Wait_For_Connect when Socket_Event (AWS.Net.Input)
                 and then Socket_Event (AWS.Net.Output);

               exit Wait_For_Connect when Wait_Until < Clock;

               delay 0.01;
            end loop Wait_For_Connect;

            if Socket_Event (AWS.Net.Input)
              and then Socket_Event (AWS.Net.Output)
            then
               Trace (Info, "Connection to "
                      & Host_Port
                      & " AMI "
                      & AMI_Connection_Type'Image (AMI_Type)
                      & " socket succeeded.");

               AMI_Callback (Socket   => Socket_List (AMI_Type),
                             Username => AMI_Username (AMI_Type),
                             Secret   => AMI_Secret (AMI_Type));
            else
               --  Socket not ready, for some reason or another. Lets do a
               --  precautionary shutdown and then try connecting again.
               AWS.Net.Buffered.Shutdown (Socket_List (AMI_Type));

               Trace (Error, "Connection to "
                      & Host_Port
                      & " AMI "
                      & AMI_Connection_Type'Image (AMI_Type)
                      & "socket failed.");
            end if;

         exception
            when Event : AWS.Net.Socket_Error =>
               pragma Unreferenced (Event);
               if not Shutdown then
                  null;
                  --  TODO: Switch to System_Message.
--                    Log_Exception
--                      (Event   => E,
--                       Message => "Lost connection to AMI "
--                       & AMI_Connection_Type'Image (AMI_Type)
--                       & " socket");
               end if;
            when Event : others =>
               pragma Unreferenced (Event);
               null;
               --  TODO: Switch to System_Message.
--                 Log_Exception
--                   (Event   => E,
--                   Message => "Error! We might've lost the connection to the"
--                    & " AMI "
--                    & AMI_Connection_Type'Image (AMI_Type)
--                & " socket. Precautionary socket shutdown under way and then"
--                    & " we'll try connecting again");

               AWS.Net.Buffered.Shutdown (Socket_List (AMI_Type));
         end;

         if Shutdown then
            exit Socket_Connection;
         end if;

         delay Reconnect_Delay;
      end loop Socket_Connection;

      Trace (Info, "AMI "
             & AMI_Connection_Type'Image (AMI_Type)
             & " socket exited its main loop."
             & " Task completion imminent.");
   end AMI_Socket;

   Action_Socket : AMI_Socket
     (AMI_Type     => Action,
      AMI_Callback => AMI.Action.Start'Access);

   Event_Socket : AMI_Socket
     (AMI_Type     => Event,
      AMI_Callback => AMI.Event.Start'Access);

   ---------------
   --  Connect  --
   ---------------

   procedure Connect
   is
   begin
      Action_Socket.Start;
      Event_Socket.Start;
   end Connect;

   ------------------
   --  Disconnect  --
   ------------------

   procedure Disconnect
   is
      use Yolk.Log;
   begin
      Shutdown := True;

      for AMI_Type in AMI_Connection_Type'Range loop
         AWS.Net.Buffered.Shutdown (Socket_List (AMI_Type));

         Trace (Info, "Shutting down connection to "
                & Host_Port
                & " AMI "
                & AMI_Connection_Type'Image (AMI_Type)
                & " socket.");
      end loop;
   end Disconnect;

end AMI.Std;
