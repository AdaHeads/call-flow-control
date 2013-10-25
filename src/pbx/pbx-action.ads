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
--  TODO: this package is in dire need of a cleanup!
with PBX.Call;
with Model.Agent;

package PBX.Action is
   use PBX;

   Package_Name : constant String := "PBX.Action";

   --  TODO add a cleanup task or housekeeping hook.

   Timeout   : exception;
   Error     : exception;

--     function Value (ID : AMI.Action_ID_Type)
--                    return Reply_Ticket;

   type Response_Handler is access procedure;

   Ignore : constant access procedure := null;
   --  Standard null-body handler for ignoring responses.

   procedure Bridge (Source      : in PBX.Call.Identification;
                     Destination : in PBX.Call.Identification);
   --  Bridges two channels. The caller must explicitly specify which legs are
   --  bridged. Raises Null_Channel if either channel is null.

   procedure Hangup (ID : in Call.Identification);
   --  Request a hangup on a given call.

--   procedure Logoff;

   procedure Update_Call_List;

--   procedure Update_SIP_Peer_List;

   procedure Originate (Agent       : in Model.Agent.Agent_Type;
                        Extension   : in String);
   --  Start originate. Raises Timeout or Error when either occurs.

   procedure Park (ID : in Call.Identification);
   procedure Transfer (Call  : in PBX.Call.Identification;
                       Agent : in Model.Agent.Agent_Type);

private

end PBX.Action;
