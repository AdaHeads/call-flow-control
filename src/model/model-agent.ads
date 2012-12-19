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
with Model.Agent_ID;
with AMI.Peer;
with AMI.Peer_ID;

package Model.Agent is
   use Ada.Strings.Unbounded;
   use Model.Agent_ID;
   use AMI.Peer;
   use AMI.Peer_ID;

   type Agent_Type is tagged private;
   type State is (Signed_Out, Idle, Paused, Away);

   function Create (ID        : in Agent_ID_Type;
                    Peer_ID   : in Peer_ID_Type;
                    Extension : in String) return Agent_Type;

   procedure Assign (Agent : in     Agent_Type;
                     Peer  :    out Peer_Type);

   procedure Change_State (Agent     :    out Agent_Type;
                           New_State : in     State);

   function Context (Agent : in Agent_Type) return String;

   function Current_State (Agent : in Agent_Type) return State;

   function Extension (Agent : in Agent_Type) return String;

   function ID (Agent : in Agent_Type) return Agent_ID_Type;

   function Peer_ID (Agent : in Agent_Type) return Peer_ID_Type;

   Null_Agent : constant Agent_Type;
   --   function Get (Agent_ID : in Agent_ID_Type) return Agent_Type;

private
   type Agent_Type is tagged
      record
         ID            : Agent_ID_Type;
         Current_State : State;
         Name          : Unbounded_String;
         Context       : Unbounded_String;
         Peer_ID       : Peer_ID_Type;
         Extension     : Unbounded_String;
      end record;

   Null_Agent : constant Agent_Type :=
                  (ID            => Null_Agent_ID,
                   Current_State => Signed_Out,
                   Name          => Null_Unbounded_String,
                   Context       => Null_Unbounded_String,
                   Peer_ID       => Null_Peer_ID,
                   Extension     => Null_Unbounded_String);
end Model.Agent;
