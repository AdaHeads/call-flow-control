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

--  Database wrapper for agents

with Model.Agent;
with Model.Agent_ID;
with AMI.Peer_ID;

package Model.Agents is
   use Model.Agent;
   use Model.Agent_ID;
   use AMI.Peer_ID;

   function Get (Agent_ID : in Agent_ID_Type) return Agent_Type;
   --  Locate an agent by Agent_ID

   function Get (Peer_ID : Peer_ID_Type) return Agent_Type;
   --  Locate an agent by Peer_ID

end Model.Agents;
