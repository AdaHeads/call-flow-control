with Ada.Calendar,
     Ada.Containers.Hashed_Maps,
     Ada.Containers.Vectors,
     Ada.Strings.Hash,
     Ada.Strings.Unbounded;

with Call_List;
package Peers is
   use Ada.Containers;
   use Ada.Strings.Unbounded;
   type SIP_Peer_Status_Type is (Unregistered, Registered);

   package Call_List is new
     Ada.Containers.Vectors (Index_Type   => Positive,
                             Element_Type => Call_List.Call_Type,
                             "="          => Call_List."=");

   type Peer_Type is
      record
         Agent_ID     : Unbounded_String;
         Defined      : Boolean := False;
         Status       : SIP_Peer_Status_Type := Unregistered;
         ChannelType  : Unbounded_String;
         Peer         : Unbounded_String;
         Port         : Unbounded_String;
         Address      : Unbounded_String;
         Paused       : Boolean := False;
         Last_Seen    : Ada.Calendar.Time := Ada.Calendar.Clock;
         Exten        : Unbounded_String;

         Computer_ID : Unbounded_String;
         --  Dete skal kun symbolere de informationen, der måtte komme senere
      end record;

      null_Peer : Peer_Type :=
     (Agent_ID     => Null_Unbounded_String,
      Defined      => False,
      Status       => Unregistered,
      ChannelType  => Null_Unbounded_String,
      Peer         => Null_Unbounded_String,
      Port         => Null_Unbounded_String,
      Address      => Null_Unbounded_String,
      Paused       => False,
      Last_Seen    => Ada.Calendar.Clock,
      Exten        => Null_Unbounded_String,
      Computer_ID  => Null_Unbounded_String);

   type Peer_type_access is access Peer_Type;

   function Hash (Peer_Address : in Unbounded_String) return Hash_Type;

   package Peer_List_Type is new Ada.Containers.Hashed_Maps
     (Key_Type => Unbounded_String,
      Element_Type => Peer_type,
      Hash => Hash,
      Equivalent_Keys => "=");

   function Get_Peers_List return Peer_List_Type.Map;
   function Get_Exten (Peer : in Unbounded_String) return Unbounded_String;
   function Get_Peer (Agent_ID : in Unbounded_String) return Peer_Type;

--     procedure Print_Peer (Peer : in Peer_Type);
   procedure Insert_Peer (New_Item : in Peer_Type);
   procedure Replace_Peer (Item : in Peer_Type);

end Peers;
