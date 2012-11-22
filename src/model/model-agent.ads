--with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded;
with Model.Call;
with Model.Calls;
with Model.Call_ID;
with Model.Agent_ID;

package Model.Agent is
   use Ada.Strings.Unbounded;
   use Model.Call_ID;
   use Model.Call;
   use Model.Calls;
   use Model.Agent_ID;

   type Agent_Type is tagged
      record
         ID   : Agent_ID_Type;
         Peer : Unbounded_String;
      end record;

   function Assign (Agent   : in out Agent_Type;
                    Call_ID : in Call_ID_Type;
                    List    : in out Protected_Call_List_Type) return Call_Type;

   Null_Agent : constant Agent_Type;
   --   function Get (Agent_ID : in Agent_ID_Type) return Agent_Type;

private
   Null_Agent : constant Agent_Type := 
     (ID   => Null_Agent_ID,
      Peer => Null_Unbounded_String);
end Model.Agent;
