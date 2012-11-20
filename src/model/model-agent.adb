package body Agent is
   
   function Assign (Agent   : in out Agent_Type;
                    Call_ID : in Call_ID_Type) return Call_Type is
      Call : Call_Type := Call_List.Get (Call_ID);
   begin
      Call.Assigned_To := Agent.ID;
      return Call_List.Update (Call);
   end Assign;
end Agent;
   
