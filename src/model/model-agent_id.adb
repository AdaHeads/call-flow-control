package body Model.Agent_ID is
   function Create (Agent_ID : in String) return Agent_ID_Type is
   begin
      return (ID => Natural'Value (Agent_ID));
   end Create;

   function To_String (Agent_ID : in Agent_ID_Type) return String is
   begin
      if Agent_ID = Null_Agent_ID then
         return "<null>";
      else
         return Agent_ID.ID'Img;
      end if;
   end To_String;
end Model.Agent_ID;
