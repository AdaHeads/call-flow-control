-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                             Call_Queue_JSON                               --
--                                                                           --
--                                  SPEC                                     --
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

with Model.Peers;
with Model.Call;

package JSON.Event is
   use Common;
   use Model.Call;
   use Model.Peers;

   --  -------------------  --
   --  Call related events  --
   --  -------------------  --

   function Hangup_JSON_String (Call : in Call_Type)
                               return JSON_String;
   function New_Call_JSON_String (Call : in Call_Type)
                                 return JSON_String;

   function Pickup_Call_JSON_String (Call  : in Call_Type;
                                     Agent : in Peer_Type)
                                return JSON_String;

   function Hold_Call_JSON_String (Call  : in Call_Type)
                                  return JSON_String;

   function Transfer_Call_JSON_String (Call  : in Call_Type)
                                      return JSON_String;

   --  --------------------  --
   --  Agent related events  --
   --  --------------------  --

   function Agent_State_JSON_String (Agent : in Peer_Type)
                                    return JSON_String;
end JSON.Event;
