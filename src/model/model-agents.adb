with Ada.Strings.Unbounded;

package body Model.Agents is
   use Ada.Strings.Unbounded;
   use Model;
   function Get (Agent_ID : in Agent_ID_Type) return Agent_Type is
   begin
      if Agent_ID.ID = 1 then
         return Agent.Create
           (ID        => Model.Agent_ID.Create ("1"),
            Peer_ID   => Model.Peer_ID.Create ("SIP/softphone1"),
            Extension => To_Unbounded_String ("101"));
      else
         return Null_Agent;
      end if;
   end Get;

   --  TODO: Change this to a real database.
   function Lookup (Peer_ID : Peer_ID_Type) return Agent_Type is

   begin
      if Peer_ID.Peername = "softphone1" then
         return Get (Agent_ID.Create ("1"));
      elsif Peer_ID.Peername = "softphone2" then
         return Agent.Create
           (ID        => Agent_ID.Create ("2"),
            Peer_ID   => Peer_ID,
            Extension => To_Unbounded_String ("102"));
      end if;
      return Null_Agent;
   end Lookup;

end Model.Agents;
