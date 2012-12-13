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

-- Deprecated versions of AMI action packets.

package AMI.Packet.Action.Deprecated is

   function Agent_Callback_Login
     (Agent     : in String;
      --  Agent ID of the agent to log in to the system,
      --  as specified in agents.conf.
      Extension : String;
      --  Extension to use for callback.
      Context   : String := "";
      Acknlowledgde_Call : Boolean := False;
      --  Set to true to require an acknowledgement (the agent pressing the
      --  # key) to accept the call when agent is called back.
      WrapupTime         : Duration := Duration'First;
      On_Response        : in     Response_Handler_Type
      := Null_Reponse_Handler'Access
      --  The reponse handler
     ) return Request;
   --  DEPRECATED as of 1.6, and removed in 1.8.
   --  Sets an agent as logged in to the queue system in callback mode
   --  Logs the specified agent in to the Asterisk queue system in callback
   --  mode. When a call is distributed to this agent,
   --  it will ring the specified extension.

end AMI.Packet.Action.Deprecated;
