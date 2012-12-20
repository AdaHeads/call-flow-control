-------------------------------------------------------------------------------
--                                                                           --
--                     Copyright (C) 2012-, AdaHeads K/S                     --
--                                                                           --
--  This is free software;  you can redistribute it and/or modify it         --
--  under terms of the  GNU General Public License  as published by the      --
--  Free Software  Foundation;  either version 3,  or (at your  option) any  --
--  later version. This library is distributed in the hope that it will be   --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     --
--  You should have received a copy of the GNU General Public License and    --
--  a copy of the GCC Runtime Library Exception along with this program;     --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
--  <http://www.gnu.org/licenses/>.                                          --
--                                                                           --
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded;

package body Model.Agents is
   use Ada.Strings.Unbounded;

   function Get (Agent_ID : in Agent_ID_Type) return Agent_Type is
   begin
      case Agent_ID.ID  is
         when 1 =>
            return Agent.Create
              (ID        => Agent_ID,
               Peer_ID   => AMI.Peer_ID.Create ("SIP/softphone1"),
               Extension => "101");
         when 2 =>
            return Agent.Create
              (ID        => Agent_ID,
               Peer_ID   =>  AMI.Peer_ID.Create ("SIP/softphone1"),
               Extension => "102");
         when 3 =>
            return Agent.Create
              (ID        => Agent_ID,
               Peer_ID   =>  AMI.Peer_ID.Create ("SIP/softphone1"),
               Extension => "103");
         when 4 =>
            return Agent.Create
           (ID        => Agent_ID,
            Peer_ID   =>  AMI.Peer_ID.Create ("SIP/softphone1"),
            Extension => "104");
         when 5 =>
            return Agent.Create
           (ID        => Agent_ID,
            Peer_ID   =>  AMI.Peer_ID.Create ("SIP/softphone1"),
            Extension => "105");
         when 6 =>
            return Agent.Create
           (ID        => Agent_ID,
            Peer_ID   =>  AMI.Peer_ID.Create ("SIP/softphone1"),
            Extension => "106");
         when 7 =>
            return Agent.Create
              (ID        => Agent_ID,
               Peer_ID   =>  AMI.Peer_ID.Create ("SIP/uhh"),
               Extension => "107");
         when others =>
            return Null_Agent;
      end case;
   end Get;

   --  TODO: Change this to a real database.
   function Get (Peer_ID : Peer_ID_Type) return Agent_Type is

   begin
      if Peer_ID.Peername = "softphone1" then
         return Get (Agent_ID.Create ("1"));

      elsif Peer_ID.Peername = "softphone2" then
         return Get (Agent_ID.Create ("2"));

      elsif Peer_ID.Peername = "TP-Softphone" then
         return Get (Agent_ID.Create ("3"));

      elsif Peer_ID.Peername = "JSA-N900" then
         return Get (Agent_ID.Create ("4"));

      elsif Peer_ID.Peername = "DesireZ" then
         return Get (Agent_ID.Create ("5"));

      elsif Peer_ID.Peername = "TL-Softphone" then
         return Get (Agent_ID.Create ("6"));

      elsif Peer_ID.Peername = "uhh" then
         return Get (Agent_ID.Create ("7"));
      end if;

      return Null_Agent;
   end Get;

end Model.Agents;
