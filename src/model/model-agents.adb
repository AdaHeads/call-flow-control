package body Model.Agents is
   function Get (Agent_ID : in Agent_ID_Type) return Agent_Type is
   begin
      --  case Agent_ID is
      --     when others =>
      --        return Null_Agent;
      --  end case;
      return Null_Agent;
   end Get;
   
   function Lookup (Peer_ID : Unbounded_String) return Agent_Type is
   begin
      if Peer_ID = "softphone1" then
         return (1, To_Unbounded_String ("softphone1"));
      elsif Peer_ID = "DesireZ" then
         return (2, To_Unbounded_String ("DesireZ"));
      else
         return (0, To_Unbounded_String ("Unknown"));
      end if;
   end Lookup;

end Model.Agents;
