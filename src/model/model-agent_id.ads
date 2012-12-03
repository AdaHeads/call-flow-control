package Model.Agent_ID is
   type Agent_ID_Type is tagged record
      ID : Natural := 0;
   end record;
   Null_Agent_ID : constant Agent_ID_Type;

   function To_String (Agent_ID : in Agent_ID_Type) return String;

   function Create (Agent_ID : in String) return Agent_ID_Type;

private
   Null_Agent_ID : constant Agent_ID_Type := (ID => Natural'Last);
end Model.Agent_ID;
