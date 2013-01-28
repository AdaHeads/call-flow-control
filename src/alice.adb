-------------------------------------------------------------------------------
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

with AGI.Callbacks; --  Initializes with a call to AMI.Observers.Register.
pragma Unreferenced (AGI.Callbacks);

with Alice_Handlers;
with PBX;
with System_Message.Critical;
with System_Message.Info;
with Unexpected_Exception;

with Yolk.Process_Control;
with Yolk.Server;

procedure Alice is
   use System_Message;
   use Yolk.Process_Control;
   use Yolk.Server;

   Alice_Version : constant String := "0.40";

   Web_Server : HTTP := Create
     (Unexpected => Unexpected_Exception.Callback);
begin
   PBX.Start;
   Web_Server.Start (Dispatchers => Alice_Handlers.Get);

   Info.Alice_Start (Message => "Server version " & Alice_Version);

   Wait;
   --  Wait here until we get a SIGINT, SIGTERM or SIGPWR.

   Web_Server.Stop;
   PBX.Stop;

   Info.Alice_Stop;
exception
   when Event : others =>
      Critical.Alice_Shutdown_With_Exception (Event);
      Web_Server.Stop;
      PBX.Stop;
end Alice;
