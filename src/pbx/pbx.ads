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

private
with ESL.Basic_Client;

package PBX is

   Package_Name : constant String := "PBX";

   type PBX_Status_Type is (Shut_Down, Shutting_Down, Running, Connecting,
                            Failed);

   procedure Start;
   --  Startup the PBX subsystem

   procedure Stop;
   --  Stop the PBX subsystem.

   function Status return PBX_Status_Type;
   --  Retrieve the current status of the

   procedure Connect;
   --  Wraps the connection and wait mechanism and provides a neat callback
   --  for the On_Disconnect event in the ESL.Client.

private
   type Reply_Ticket is tagged null record;

   Connection_Delay        : Duration     := 1.0;
   Shutdown                : Boolean      := False;

   Client                  : ESL.Basic_Client.Instance;
   Event_Client            : ESL.Basic_Client.Instance;

end PBX;
