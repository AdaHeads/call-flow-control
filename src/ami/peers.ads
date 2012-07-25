with Ada.Containers.Hashed_Maps;  use Ada.Containers;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Calendar;
with Call_Queue;
with Ada.Containers.Vectors;
--  TODO: Make this into a generic that uses protected types
--          for accessing the map.
--  TODO: Add "calls on hold" list.
package Peers is

   type SIP_Peer_Status_Type is (Unregistered, Registered);

   package Call_List is new
     Ada.Containers.Vectors (Index_Type   => Positive,
                             Element_Type => Call_Queue.Call_Type,
                             "="          => Call_Queue."=");

   type Peer_Type is
      record
         Defined      : Boolean := False;
         Status       : SIP_Peer_Status_Type := Unregistered;
         ChannelType  : Unbounded_String;
         Peer         : Unbounded_String;
         Port         : Unbounded_String;
         Address      : Unbounded_String;
         Paused       : Boolean;
         Last_Seen    : Ada.Calendar.Time := Ada.Calendar.Clock;
         Call         : Call_Queue.Call_Type;
         Exten        : Unbounded_String;
         Parked_Calls : Call_List.Vector;

         Computer_ID : Unbounded_String;
         --  Det skulle kun symbolere de informationen, der måtte komme senere
      end record;

   type Peer_type_access is access Peer_Type;

   function Hash (Peer_Address : in Unbounded_String) return Hash_Type;

   package Peer_List_Type is new Ada.Containers.Hashed_Maps
     (Key_Type => Unbounded_String,
      Element_Type => Peer_type,
      Hash => Hash,
      Equivalent_Keys => "=");

   function Get_Exten (Peer : in Unbounded_String) return Unbounded_String;
   procedure Print_Peer (Peer : in Peer_Type);
end Peers;
