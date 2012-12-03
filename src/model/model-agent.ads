with Ada.Strings.Unbounded;
with Model.Call;
with Model.Calls;
with Model.Call_ID;
with Model.Agent_ID;
with Model.Peer_ID;

package Model.Agent is
   use Ada.Strings.Unbounded;
   use Model.Agent_ID;
   use Model.Call;
   use Model.Calls;
   use Model.Call_ID;
   use Model.Peer_ID;
   use Model;

   type Agent_Type is tagged
      record
         ID        : Agent_ID_Type;
         Peer_ID   : Peer_ID_Type;
         Extension : Unbounded_String;
      end record;

   function Assign (Agent   : in out Agent_Type;
                    Call_ID : in Call_ID_Type;
                    List    : in out Protected_Call_List_Type)
                    return Call_Type;

   function Create (ID        : in Agent_ID_Type;
                    Peer_ID   : in Peer_ID_Type;
                    Extension : in Unbounded_String) return Agent_Type;

   Null_Agent : constant Agent_Type;
   --   function Get (Agent_ID : in Agent_ID_Type) return Agent_Type;

private
   Null_Agent : constant Agent_Type :=
     (ID        => Null_Agent_ID,
      Peer_ID   => Null_Peer_ID,
      Extension => Null_Unbounded_String);
end Model.Agent;
