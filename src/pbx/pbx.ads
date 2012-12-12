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

with AMI.Client;
with Common;

package PBX is
   use AMI;

   type PBX_Status_Type is (Shutdown, Shutting_Down, Running, Connecting,
                           Failure);

   --  TODO: make this private and wrap every call to AMI
   Client        : aliased AMI.Client.Client_Type;
   Client_Access : constant access AMI.Client.Client_Type := Client'Access;

   procedure Start;
   --  Startup the PBX subsystem

   procedure Stop;
   --  Stop the PBX subsystem.

   function Status return PBX_Status_Type;
   --  Retrieve the current status of the

private
   Last_Connection_Attempt : Common.Time;
   Connection_Delay        : Duration        := 1.0;
   PBX_Status              : PBX_Status_Type := Failure;
end PBX;
