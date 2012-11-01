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

with Ada.Calendar;
with Common;

with Peers;
with Call_List;

package Event_JSON is
   use Common;

   function Hangup_JSON_String (Call : in Call_List.Call_Type)
                               return JSON_String;
   function New_Call_JSON_String (Call : in Call_List.Call_Type)
                                return JSON_String;
   
   function Pickup_Call_JSON_String (Call  : in Call_List.Call_Type;
                                     Agent : in Peers.Peer_Type)
                                return JSON_String;
   
   function Hold_Call_JSON_String (Call  : in Call_List.Call_Type)
                                  return JSON_String;
   
   function Transfer_Call_JSON_String (Call  : in Call_List.Call_Type)
                                      return JSON_String;

private
   --  function To_JSON_Object (Call : in Call_List.Call_Type)
   --                           return GNATCOLL.JSON.JSON_Value;
   --  takes a call and converts it to a JSON object.
   
   function Unix_Timestamp
     (Date : in Ada.Calendar.Time)
     return String;
   --  Convert and trim an Ada.Calendar.Time type to a Unix timestamp
   --  String.
end Event_JSON;
