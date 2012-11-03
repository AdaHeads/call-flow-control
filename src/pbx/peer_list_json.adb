with Ada.Calendar.Conversions;
with Ada.Calendar.Formatting;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Characters.Handling;

private with GNATCOLL.JSON;

with Interfaces.C;
with System_Messages; use System_Messages;

package body Peer_List_JSON is
   use GNATCOLL.JSON;
   use Ada.Strings.Unbounded;
   use Ada.Characters.Handling;

   function To_JSON_Object (Peer : in Peers.Peer_Type)
                           return GNATCOLL.JSON.JSON_Value;
   
   function To_JSON_Object (Peer_List : in Peers.Peer_List_Type.Map)
                           return GNATCOLL.JSON.JSON_Value;

   
   function To_JSON_String (Peer : in Peers.Peer_Type)
                             return JSON_String is
      JSON : JSON_Value;
   begin
      JSON := To_JSON_Object (Peer);

      return To_JSON_String (JSON.Write);
   end To_JSON_String;
      
   function To_JSON_String (Peer_List : in Peers.Peer_List_Type.Map)
                                  return JSON_String is
      JSON : JSON_Value;
   begin
      JSON := To_JSON_Object (Peer_List);

      return To_JSON_String (JSON.Write);
   end To_JSON_String;

   function To_JSON_Object (Peer : in Peers.Peer_Type)
                            return GNATCOLL.JSON.JSON_Value is
      use Ada.Calendar.Formatting;

      JSON      : constant JSON_Value := Create_Object;
      Peer_JSON : constant JSON_Value := Create_Object;
   begin
      System_Messages.Notify (Debug, Peers.Image (Peer) );
      Peer_JSON.Set_Field ("Agent_ID", To_String(Peer.Agent_Id));
      Peer_JSON.Set_Field ("Defined", Peer.Defined);
      Peer_JSON.Set_Field ("State", To_Lower(Peer.State'Img));
      Peer_JSON.Set_Field ("Last_State", To_Lower(Peer.Last_State'Img));
      Peer_JSON.Set_Field ("ChannelType", To_String (Peer.ChannelType));
      Peer_JSON.Set_Field ("Peer", To_String (Peer.Peer));
      Peer_JSON.Set_Field ("Port", To_String (Peer.Port));
      Peer_JSON.Set_Field ("Address", To_String (Peer.Address));
      Peer_JSON.Set_Field ("Paused", Peer.Paused);
      Peer_JSON.Set_Field ("Last_Seen", Ada.Calendar.Formatting.Image (Peer.Last_Seen));
      Peer_JSON.Set_Field ("Exten", Peer.Exten);
      JSON.Set_Field ("peer", Peer_JSON);
      JSON.Set_Field ("timestamp", Unix_Timestamp (Current_Time));

      return JSON;
   end To_JSON_Object;      
   
   function To_JSON_Object (Peer_List : in Peers.Peer_List_Type.Map)
                           return GNATCOLL.JSON.JSON_Value is
      JSON_List : JSON_Array;
      Value     : JSON_Value;

      Result : constant JSON_Value := Create_Object;
   begin
      JSON_List := Empty_Array;

      for Agent of Peer_List loop
         Value := To_JSON_Object (Agent);
         Append (JSON_List, Value);
      end loop;

      Result.Set_Field ("agents", JSON_List);

      return Result;
   end To_JSON_Object;

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
end Peer_List_JSON;
