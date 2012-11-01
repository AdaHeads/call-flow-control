-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                              Call_Queue_JSON                              --
--                                                                           --
--                                  BODY                                     --
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

with Ada.Calendar.Conversions;
with Ada.Strings.Fixed;
--with Ada.Strings.Unbounded;

private with GNATCOLL.JSON;


with Interfaces.C;
package body Event_JSON is
   use GNATCOLL.JSON;

   function Current_Time return Ada.Calendar.Time renames Ada.Calendar.Clock;
   
   --  Length_String : constant String := "length";
   
   function Hangup_Call_To_JSON_Object (Call : in Call_List.Call_Type)
                                       return GNATCOLL.JSON.JSON_Value;
   
   function New_Call_To_JSON_Object (Call : in Call_List.Call_Type)
                           return GNATCOLL.JSON.JSON_Value;
   
   function Pickup_Call_To_JSON_Object (Call  : in Call_List.Call_Type;
                                        Agent : in Peers.Peer_Type)
                                    return GNATCOLL.JSON.JSON_Value;
   
   function Hold_Call_To_JSON_Object (Call : in Call_List.Call_Type)
                                     return GNATCOLL.JSON.JSON_Value;
   
   function Transfer_Call_To_JSON_Object (Call : in Call_List.Call_Type)
                                     return GNATCOLL.JSON.JSON_Value;
   
   --  ---------------------  --
   --  JSON String functions  --
   --  ---------------------  --
   
   function Hangup_JSON_String (Call : in Call_List.Call_Type)
                                return JSON_String is
      JSON : JSON_Value;
   begin
      JSON := Hangup_Call_To_JSON_Object (call);

      return To_JSON_String (JSON.Write);
   end Hangup_JSON_String;
   
   function New_Call_JSON_String (Call : in Call_List.Call_Type)
                                return JSON_String is
      JSON : JSON_Value;
   begin
      JSON := New_Call_To_JSON_Object (Call);

      return To_JSON_String (JSON.Write);
   end New_Call_JSON_String;
   
   function Pickup_Call_JSON_String (Call  : in Call_List.Call_Type;
                                     Agent : in Peers.Peer_Type)
                                    return JSON_String is
      JSON : JSON_Value;
   begin
      JSON := Pickup_Call_To_JSON_Object (Call,Agent);

      return To_JSON_String (JSON.Write);
   end Pickup_Call_JSON_String;

   function Hold_Call_JSON_String (Call  : in Call_List.Call_Type)
                                  return JSON_String is
      JSON : JSON_Value;
   begin
      JSON := Hold_Call_To_JSON_Object (Call);

      return To_JSON_String (JSON.Write);
   end Hold_Call_JSON_String;
   
   function Transfer_Call_JSON_String (Call  : in Call_List.Call_Type)
                                      return JSON_String is
      JSON : JSON_Value;
   begin
      JSON := Transfer_Call_To_JSON_Object (Call);

      return To_JSON_String (JSON.Write);
   end Transfer_Call_JSON_String;

   --  ---------------------  --
   --  JSON Object functions  --
   --  ---------------------  --
   
   function New_Call_To_JSON_Object (Call : in Call_List.Call_Type)
                           return GNATCOLL.JSON.JSON_Value is
      JSON              : constant JSON_Value := Create_Object;
      Notification_JSON : constant JSON_Value := Create_Object;
      Call_JSON         : constant JSON_Value := Create_Object;
   begin
      Call_JSON.Set_Field ("call_id", Call.Uniqueid);
      Call_JSON.Set_Field ("caller_id", Call.Agent_ID);
      Call_JSON.Set_Field ("arrival_time", Unix_Timestamp (Call.Arrived));
      Call_JSON.Set_Field ("channel", Call.Channel);
      Call_JSON.Set_Field ("org_id", Call.CallerIDNum);
      Notification_JSON.Set_Field ("call", Call_JSON);
      Notification_JSON.Set_Field ("persistent", False);
      Notification_JSON.Set_Field ("event", "new_call");
      
      JSON.Set_Field ("timestamp", Unix_Timestamp (Current_Time));
      JSON.Set_Field ("notification", Notification_JSON);

      return JSON;
   end New_Call_To_JSON_Object;
   
   
   --  {
   --    "notification" : {
   --       "persistent" : false,
   --       "event" : "call_pickup",
   --       "agent" : { "agent_id" : "SomeAgent_ID" },
   --       "call" : { "call_id" : "SomeCall_ID" }
   --  }   
   function Pickup_Call_To_JSON_Object (Call  : in Call_List.Call_Type;
                                        Agent : in Peers.Peer_Type)
                                    return GNATCOLL.JSON.JSON_Value is
      JSON              : constant JSON_Value := Create_Object;
      Notification_JSON : constant JSON_Value := Create_Object;
      Call_JSON         : constant JSON_Value := Create_Object;
      Agent_JSON        : constant JSON_Value := Create_Object;
   begin
      Call_JSON.Set_Field ("call_id", Call.Uniqueid);
      Notification_JSON.Set_Field ("call", Call_JSON);
      Notification_JSON.Set_Field ("persistent", False);
      Notification_JSON.Set_Field ("event", "pickup_call");
      Agent_JSON.Set_Field ("agent_id", Agent.Agent_ID);
      Notification_JSON.Set_Field ("agent", Agent_JSON);
      
      JSON.Set_Field ("timestamp", Unix_Timestamp (Current_Time));
      JSON.Set_Field ("notification", Notification_JSON);

      return JSON;
   end Pickup_Call_To_JSON_Object;
   
   function Hangup_Call_To_JSON_Object (Call : in Call_List.Call_Type)
                           return GNATCOLL.JSON.JSON_Value is
      JSON              : constant JSON_Value := Create_Object;
      Notification_JSON : constant JSON_Value := Create_Object;
      Call_JSON         : constant JSON_Value := Create_Object;
   begin
      Call_JSON.Set_Field ("call_id", Call.Uniqueid);
      Notification_JSON.Set_Field ("call", Call_JSON);
      Notification_JSON.Set_Field ("persistent", False);
      Notification_JSON.Set_Field ("event", "hangup_call");
      
      JSON.Set_Field ("timestamp", Unix_Timestamp (Current_Time));
      JSON.Set_Field ("notification", Notification_JSON);

      return JSON;
   end Hangup_Call_To_JSON_Object;

   -- Example JSON
   --  {
   --      "notification": {
   --          "persistent": false,
   --          "event": "hold_call",
   --          "call": {
   --              "call_id": 1
   --          }
   --      }
   --  }
   function Hold_Call_To_JSON_Object (Call : in Call_List.Call_Type)
                           return GNATCOLL.JSON.JSON_Value is
      JSON              : constant JSON_Value := Create_Object;
      Notification_JSON : constant JSON_Value := Create_Object;
      Call_JSON         : constant JSON_Value := Create_Object;
   begin
      Call_JSON.Set_Field ("call_id", Call.Uniqueid);
      Notification_JSON.Set_Field ("call", Call_JSON);
      Notification_JSON.Set_Field ("persistent", False);
      Notification_JSON.Set_Field ("event", "hold_call");

      JSON.Set_Field ("timestamp", Unix_Timestamp (Current_Time));
      JSON.Set_Field ("notification", Notification_JSON);

      return JSON;
   end Hold_Call_To_JSON_Object;

   function Transfer_Call_To_JSON_Object (Call : in Call_List.Call_Type)
                           return GNATCOLL.JSON.JSON_Value is
      JSON              : constant JSON_Value := Create_Object;
      Notification_JSON : constant JSON_Value := Create_Object;
      Call_JSON         : constant JSON_Value := Create_Object;
   begin
      Call_JSON.Set_Field ("call_id", Call.Uniqueid);
      Call_JSON.Set_Field ("transfered_to", "NOT IMPLEMENTED");
      Notification_JSON.Set_Field ("call", Call_JSON);
      Notification_JSON.Set_Field ("persistent", False);
      Notification_JSON.Set_Field ("event", "transfer_call");

      JSON.Set_Field ("timestamp", Unix_Timestamp (Current_Time));
      JSON.Set_Field ("notification", Notification_JSON);

      return JSON;
   end Transfer_Call_To_JSON_Object;
   
   --  -----------------  --
   --  Utility functions  --
   --  -----------------  --
   
   function Unix_Timestamp
     (Date : in Ada.Calendar.Time)
     return String
   is
      use Ada.Calendar;
      use Ada.Calendar.Conversions;
      use Ada.Strings;
      use Interfaces.C;
   begin
      return Fixed.Trim
        (Source => long'Image (To_Unix_Time (Date)),
         Side   => Left);
   end Unix_Timestamp;
end Event_JSON;
