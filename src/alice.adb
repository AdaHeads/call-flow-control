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

with My_Handlers;
with Yolk.Configuration;
with Yolk.Process_Control;
with Yolk.Process_Owner;
with Yolk.Server;
with Yolk.Whoops;
with System_Message.Critical;
with System_Message.Info;

procedure Alice is
   use System_Message;
   use Yolk.Configuration;
   use Yolk.Process_Control;
   use Yolk.Process_Owner;
   use Yolk.Server;

   Alice_Version : constant String := "0.40";

   Web_Server    : HTTP := Create
     (Set_Dispatchers => My_Handlers.Set'Access,
      Unexpected      => Yolk.Whoops.Unexpected_Exception_Handler'Access);
begin
   Set_User (Config.Get (Yolk_User));

   Web_Server.Start;

   Notify (Info.Alice_Startup,
           "Server version "
           & Alice_Version);

   Wait;
   --  Wait here until we get a SIGINT, SIGTERM or SIGPWR.

   Web_Server.Stop;

   Notify (Info.Alice_Stop);
exception
   when Event : Username_Does_Not_Exist =>
      Notify (Critical.Unknown_User, Event);
   when Event : others =>
      Notify (Critical.Alice_Shutdown_With_Exception, Event);
      Web_Server.Stop;
end Alice;
