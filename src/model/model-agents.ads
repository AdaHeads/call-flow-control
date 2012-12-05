with Model.Agent;
with Model.Agent_ID;
with Model.Peer_ID;

package Model.Agents is
   use Model.Agent;
   use Model.Agent_ID;
   use Model.Peer_ID;

   function Get (Agent_ID : in Agent_ID_Type) return Agent_Type;
   --  Returns the agent with the given ID

   function Lookup (Peer_ID : Peer_ID_Type) return Agent_Type;

end Model.Agents;
