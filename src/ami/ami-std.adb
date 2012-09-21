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

with AMI.Action;
with AMI.Event;
with AWS.Net.Std;
with AWS.Net.Buffered;
with Errors;
with My_Configuration;
with Yolk.Log;

package body AMI.Std is

   use My_Configuration;

   type AMI_Connection_Type is (Action, Event);

   type Callback is access procedure (Socket   : in AWS.Net.Std.Socket_Type;
                                      Username : in String;
                                      Secret   : in String);

   type String_Ptr is not null access all String;

   Action_Secret     : aliased String := Config.Get (PBX_Action_Secret);
   Action_User       : aliased String := Config.Get (PBX_Action_User);
   Event_Secret      : aliased String := Config.Get (PBX_Event_Secret);
   Event_User        : aliased String := Config.Get (PBX_Event_User);

   Action_Secret_Ptr : constant String_Ptr := Action_Secret'Access;
   Action_User_Ptr   : constant String_Ptr := Action_User'Access;
   Event_Secret_Ptr  : constant String_Ptr := Event_Secret'Access;
   Event_User_Ptr    : constant String_Ptr := Event_User'Access;

   Host_Port         : constant String := Config.Get (PBX_Host) &
                         ":" &
                         Config.Get (PBX_Port);
   Socket_List       : array (AMI_Connection_Type) of AWS.Net.Std.Socket_Type;
   Shutdown          : Boolean := False;

   task type AMI_Socket
     (AMI_Type     : AMI_Connection_Type;
      AMI_Callback : Callback;
      Secret       : String_Ptr;
      User         : String_Ptr)
   is
      entry Start;
      --  TODO: Write comment.
   end AMI_Socket;
   --  TODO: Write comment.

   ------------------
   --  AMI_Socket  --
   ------------------

   task body AMI_Socket
   is
      use Errors;
      use Yolk.Log;

      Connect_Delay   : constant Duration := 0.5;
      Host            : constant String := Config.Get (PBX_Host);
      Port            : constant Positive := Config.Get (PBX_Port);
      Socket_Event    : AWS.Net.Event_Set;
   begin
      accept Start;

      Socket_Connection :
      loop
         begin
            AWS.Net.Std.Connect (Socket => Socket_List (AMI_Type),
                                 Host   => Host,
                                 Port   => Port,
                                 Wait   => False);

            delay Connect_Delay;
            --  TODO: Make this delay a configuration parameter. We could be
            --  trying to connect to an exceptionally slow server, in which
            --  case being able to tweak this value is probably a good idea.

            Socket_Event := AWS.Net.Check
              (Socket  => Socket_List (AMI_Type),
               Events  => (AWS.Net.Input => True, AWS.Net.Output => True));

            if Socket_Event (AWS.Net.Input)
              and then Socket_Event (AWS.Net.Output)
            then
               Trace (Info, "Connection to "
                      & Host_Port
                      & " AMI "
                      & AMI_Connection_Type'Image (AMI_Type)
                      & " socket succeeded.");

               AMI_Callback (Socket   => Socket_List (AMI_Type),
                             Username => User.all,
                             Secret   => Secret.all);
            else
               --  Socket not ready, for some reason or another. Lets do a
               --  precautionary shutdown and then try connecting again.
               AWS.Net.Buffered.Shutdown (Socket_List (AMI_Type));

               Error_Handler ("Connection to "
                              & Host_Port
                              & " AMI "
                              & AMI_Connection_Type'Image (AMI_Type)
                              & "socket failed.");
            end if;

         exception
            when E : AWS.Net.Socket_Error =>
               if not Shutdown then
                  Error_Handler
                    (Event   => E,
                     Message => "Lost connection to AMI "
                     & AMI_Connection_Type'Image (AMI_Type)
                     & " socket");
               end if;
            when E : others =>
               Error_Handler
                 (Event   => E,
                  Message => "Error! We might've lost the connection to the"
                  & " AMI "
                  & AMI_Connection_Type'Image (AMI_Type)
                  & " socket. Precautionary socket shutdown under way and then"
                  & " we'll try connecting again");

               AWS.Net.Buffered.Shutdown (Socket_List (AMI_Type));
         end;

         if Shutdown then
            exit Socket_Connection;
         end if;
      end loop Socket_Connection;

      Trace (Info, "AMI "
             & AMI_Connection_Type'Image (AMI_Type)
             & " socket exited its main loop."
             & " Task completion imminent.");
   end AMI_Socket;

   Action_Socket : AMI_Socket
     (AMI_Type     => Action,
      AMI_Callback => AMI.Action.Start'Access,
      Secret       => Action_Secret_Ptr,
      User         => Action_User_Ptr);

   Event_Socket : AMI_Socket
     (AMI_Type     => Event,
      AMI_Callback => AMI.Event.Start'Access,
      Secret       => Event_Secret_Ptr,
      User         => Event_User_Ptr);

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
