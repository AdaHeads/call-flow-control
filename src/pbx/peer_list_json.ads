with Ada.Calendar;
with Common;

with Peers;

-- Utility functions for converting peers and peer lists to JSON
package Peer_List_JSON is
   use Common;
   
   function To_JSON_String (Peer : in Peers.Peer_Type)
                             return JSON_String;
   
   function To_JSON_String (Peer_List : in Peers.Peer_List_Type.Map)
                                  return JSON_String;
private 
   function Unix_Timestamp
     (Date : in Ada.Calendar.Time)
     return String;
end Peer_List_JSON;
