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

--  This is our PBX-boundry layer. It levels out the differences between
--  various PBX communication protocols.

with PBX.Call;
with Model.User;

package PBX.Action is
   use PBX;

   Package_Name : constant String := "PBX.Action";

   Timeout   : exception;
   Error     : exception;

   procedure Bridge (Source      : in PBX.Call.Identification;
                     Destination : in PBX.Call.Identification);
   --  Bridges two channels. The caller must explicitly specify which legs are
   --  bridged. Raises Null_Channel if either channel is null.

   procedure Hangup (ID : in Call.Identification);
   --  Request a hangup on a given call.

   procedure Logoff;
   --  Do a clean logoff by sending a logoff command to the PBX.

   procedure Update_Call_List;
   --  Updates the peer list by synchronously requesting the current call list
   --  could be preceeded by a purging of the list

   procedure Update_SIP_Peer_List;

   procedure Originate (User      :  in Model.User.Instance;
                        Extension : in String);
   --  Start originate. Raises Timeout or Error when either occurs.

   procedure Park (Call : in PBX.Call.Identification;
                   User :  in Model.User.Instance);

   procedure Transfer (Call : in PBX.Call.Identification;
                       User : in Model.User.Instance);

end PBX.Action;
