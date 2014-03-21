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
--  the PBX communication, and our domain.

with Model.Call;
with Model.User;

package PBX.Action is
   use Model;

   Package_Name : constant String := "PBX.Action";

   Timeout   : exception;
   Error     : exception;

   procedure Bridge (Source      : in Call.Identification;
                     Destination : in Call.Identification);
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

   function Originate (Contact_ID   : in Model.Contact_Identifier;
                       Reception_ID : in Model.Reception_Identifier;
                       User         : in Model.User.Instance;
                       Extension    : in String)
                       return Model.Call.Identification;
   --  Start originate. Raises Timeout or Error when either occurs.

   procedure Park (Target  : in Call.Identification;
                   At_User : in User.Instance);

   procedure Transfer (Target  : in Call.Identification;
                       At_User : in User.Instance);

end PBX.Action;
