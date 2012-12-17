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
with Common;
with Ada.Characters.Handling;

package body PBX.Event.Agent_State is
   use Common;
   use Ada.Characters.Handling;

   function Create (ID        : in Agent_ID.Agent_ID_Type;
                    Old_State : in Agent.State;
                    New_State : in Agent.State) return Instance is
   begin
      return (ID        => ID,
              Old_State => Old_State,
              New_State => New_State);
   end Create;

   function To_JSON (O : in Instance) return JSON_Value is
      Content      : constant JSON_Value := Create_Object;
      Notification : constant JSON_Value := Create_Object;
   begin
      Content.Set_Field ("agent_id", O.ID.ID);
      Content.Set_Field ("persistent", False);
      Content.Set_Field ("event", "agent_state");
      Content.Set_Field ("new_state", To_Lower (O.New_State'Img));
      Content.Set_Field ("old_state", To_Lower (O.Old_State'Img));

      Notification.Set_Field ("timestamp", Unix_Timestamp (Current_Time));
      Notification.Set_Field ("notification", Content);

      return Notification;
   end To_JSON;

end PBX.Event.Agent_State;
