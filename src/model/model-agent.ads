with Ada.Containers.Hashed_Maps;
with Model.Call;
with Model.Call_ID;
with Model.Agent_ID;

package Model.Agent is
   use Model.Call_ID;
   use Model.Call;
   use Model.Agent_ID;

   type Agent_Type is tagged limited
      record
         ID : Agent_ID_Type;
      end record;
   
   function Assign (Agent   : in out Agent_Type;
                    Call_ID : in Call_ID_Type) return Call_Type;

--   function Get (Agent_ID : in Agent_ID_Type) return Agent_Type;
end Model.Agent;
