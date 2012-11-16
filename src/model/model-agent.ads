with Ada.Containers.Hashed_Maps;

package Model.Agent is

   type Agent_ID_Type is new Ada.Containers.Hash_Type;

   type Agent_Type is null record;

--   function Get (Agent_ID : in Agent_ID_Type) return Agent_Type;
end Model.Agent;
