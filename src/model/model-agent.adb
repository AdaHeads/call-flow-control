package body Model.Agent is

   function Assign (Agent   : in out Agent_Type;
                    Call_ID : in Call_ID_Type;
                    List    : in out Protected_Call_List_Type)
                    return Call_Type is
      Call : Call_Type := List.Get (Call_ID);
   begin
      Call.Assigned_To := Agent.ID;
      List.Update (Call);

      return Call;
   end Assign;
end Model.Agent;
