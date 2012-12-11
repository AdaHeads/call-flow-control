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

package body Model.Agent is

   procedure Assign (Agent : in     Agent_Type;
                     Peer  :    out Peer_Type) is
   begin
      Peer.Agent_ID := Agent.ID;
   end Assign;

   function Context (Agent : in Agent_Type) return String is
   begin
      return To_String (Agent.Context);
   end Context;

   function Create (ID        : in Agent_ID_Type;
                    Peer_ID   : in Peer_ID_Type;
                    Extension : in String) return Agent_Type is
   begin
      return (ID        => ID,
              Name      => Null_Unbounded_String,
              Context   => Null_Unbounded_String,
              Peer_ID   => Peer_ID,
              Extension => To_Unbounded_String (Extension));
   end Create;

   function Extension (Agent : in Agent_Type) return String is
   begin
      return To_String (Agent.Extension);
   end Extension;

   function ID (Agent : in Agent_Type) return Agent_ID_Type is
   begin
      return Agent.ID;
   end ID;

   function Peer_ID (Agent : in Agent_Type) return Peer_ID_Type is
   begin
      return Agent.Peer_ID;
   end Peer_ID;

end Model.Agent;
