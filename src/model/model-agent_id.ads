package Model.Agent_ID is
   type Agent_ID_Type is new Natural;

   Null_Agent_ID : constant Agent_ID_Type;
private
   Null_Agent_ID : constant Agent_ID_Type := Agent_ID_Type'Last;
end Model.Agent_ID;
