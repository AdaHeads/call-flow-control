package body Model.Agent is
   function Assign (Agent   : in out Agent_Type;
                    Call_ID : in     Call_ID_Type;
                    List    : in out Protected_Call_List_Type)
                    return Call_Type is
      Call : Call_Type := List.Get (Call_ID);
   begin
      Call.Assigned_To := Agent.ID;
      List.Update (Call);

      return Call;
   end Assign;

   function Create (ID        : in Agent_ID_Type;
                    Peer_ID   : in Peer_ID_Type;
                    Extension : in Unbounded_String) return Agent_Type is
   begin
      return (ID        => ID,
              Peer_ID   => Peer_ID,
              Extension => Extension);
   end Create;
end Model.Agent;
