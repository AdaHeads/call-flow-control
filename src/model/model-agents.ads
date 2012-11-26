with Ada.Strings.Unbounded;

with Model.Agent;
with Model.Agent_ID;

package Model.Agents is
   use Ada.Strings.Unbounded;
   use Model.Agent;
   use Model.Agent_ID;

   function Get (Agent_ID : in Agent_ID_Type) return Agent_Type;
   function Lookup (Peer_ID : Unbounded_String) return Agent_Type;

end Model.Agents;
