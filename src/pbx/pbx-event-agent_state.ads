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

with Model.Agent;
with Model.Agent_ID;

package PBX.Event.Agent_State is
   use Model;

   type Instance is new Event.Instance with
      record
         ID                   : Agent_ID.Agent_ID_Type;
         Old_State, New_State : Agent.State;
      end record;
   function To_JSON (O : in Instance) return JSON_Value;

   function Create (ID        : in Agent_ID.Agent_ID_Type;
                    Old_State : in Agent.State;
                    New_State : in Agent.State) return Instance;

end PBX.Event.Agent_State;
