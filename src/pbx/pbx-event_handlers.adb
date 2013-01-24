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

with Ada.Strings.Unbounded;
with System_Messages;

with AMI.Event;
with AMI.Parser;
with AMI.Observers;
with AMI.Channel;
with AMI.Peer;
with AMI.Peer_ID;

with Model.Agent;

package body PBX.Event_Handlers is
   use System_Messages;
   use Ada.Strings.Unbounded;
   use PBX;
   use Model;
   use AMI.Peer;

   procedure Peer_Status (Packet : in Parser.Packet_Type);
   procedure Core_Show_Channel (Packet : in Parser.Packet_Type);
   procedure Core_Show_Channels_Complete (Packet : in Parser.Packet_Type);

   procedure Queue_Abandon (Packet : in Parser.Packet_Type);

   procedure Peer_Entry (Packet : in Parser.Packet_Type);

   procedure Peer_List_Complete (Packet : in Parser.Packet_Type);

   -------------------------
   --  Core_Show_Channel  --
   -------------------------

   --  TODO: Move this to a consistency_check package, and use more
   --  of the information.
   procedure Core_Show_Channel (Packet : in Parser.Packet_Type) is
   begin
      Channel.List.Put (Key  => Packet.Get_Value (Parser.Channel),
                        Item => AMI.Channel.Create (Packet));
   end Core_Show_Channel;

   -----------------------------------
   --  Core_Show_Channels_Complete  --
   -----------------------------------

   --  Occurs at the end of a set of CoreShowChannel events.
   procedure Core_Show_Channels_Complete (Packet : in Parser.Packet_Type) is
      Context          : constant String :=
                           "Core_Show_Channels_Complete";
      Number_Of_Events : constant Natural :=
        Natural'Value (To_String (Packet.Get_Value (Parser.ListItems)));
   begin
      System_Messages.Notify (Debug, "Core_Show_Channel_Complete");
      if Number_Of_Events /= Channel.List.Length then
         System_Messages.Notify (Error, Package_Name & "." & Context & ": " &
                                "Channel list inconsistent!");
      end if;
   end Core_Show_Channels_Complete;

   ----------------
   -- Peer_Entry --
   ----------------

   procedure Peer_Entry (Packet : in Parser.Packet_Type) is
      use type Agent.State;
   --  Context : constant String := "Peer_Entry";
      Peer        : Peer_Type := Null_Peer;
   begin
      --  Fetch the peer's ID.
      Peer.ID := Peer_ID.Create
        (Channel_Kind =>
           To_String (Packet.Get_Value (Parser.ChannelType)),
         Peername     =>
           To_String (Packet.Get_Value (Parser.ObjectName)));

      --  Set the agent field
      --  TODO

      --  TODO: Update this to the whatever format Asterisk 1.8 supports.
      if To_String (Packet.Get_Value (Parser.IPaddress)) /= "-none-" then
         Peer.Address := Packet.Get_Value (Parser.IPaddress);
         Peer.Port := Packet.Get_Value (Parser.IPport);

         Peer.State := Unknown;
      else
         Peer.State := Unregistered;
      end if;

      --  Update the peer
      AMI.Peer.List.Put (Peer => Peer);

   end Peer_Entry;

   procedure Peer_List_Complete (Packet : in Parser.Packet_Type) is
      Context          : constant String :=
                           "Peer_List_Complete";
      Number_Of_Events : constant Natural :=
        Natural'Value (To_String (Packet.Get_Value (Parser.ListItems)));
   begin
      if Number_Of_Events /= AMI.Peer.List.Count then
         System_Messages.Notify (Error, Package_Name & "." & Context & ": " &
                                   "peer list inconsistent! Got" &
                                   Number_Of_Events'Img &
                                " - expected" & AMI.Peer.List.Count'Img);
      end if;
   end Peer_List_Complete;

   -----------------
   -- Peer_Status --
   -----------------

   procedure Peer_Status (Packet : in Parser.Packet_Type) is

      Context : constant String := "Peer_Status";
      Peer    : Peer_Type := Null_Peer;
   begin

      Peer.ID := Peer_ID.Create
        (To_String (Packet.Get_Value (AMI.Parser.Peer)));
      --  Check if the peer is known
      if AMI.Peer.List.Contains (Peer.ID) then
         Peer := AMI.Peer.List.Get (Peer.ID);
      else
         System_Messages.Notify (Critical,
                                 Package_Name & "." & Context &
                                 ": got unknown peer " & Peer.ID.To_String);
      end if;

      --  Find the agent, and assign the peer.
      --  TODO

      --  Update fields
      Peer.Seen; --  Bump timstamp.
      if Packet.Has_Value (Parser.Address) then
         Peer.Address := Packet.Get_Value (Parser.Address);
      end if;

      if Packet.Has_Value (AMI.Parser.Port) then
         Peer.Port := Packet.Get_Value (Parser.Port);
      end if;

      --  Setting the State - registered or not.
      if Packet.Has_Value (AMI.Parser.PeerStatus) then
         --  Save the previous state.
         Peer.Last_State := Peer.State;
         if To_String (Packet.Get_Value (Parser.PeerStatus)) =
           AMI.Peer_State_Unregistered then
            Peer.State := Unregistered;

         elsif To_String (Packet.Get_Value (Parser.PeerStatus)) =
           AMI.Peer_State_Registered then
            Peer.State := Idle;

         else
            Peer.State := Unknown;
            System_Messages.Notify
              (Critical, "My_Callbacks.Peer_Status: " &
                 "Peer changed state into an unknown state: " &
                 To_String (Packet.Get_Value (Parser.PeerStatus)));
         end if;

         System_Messages.Notify
           (Debug, "My_Callbacks.Peer_Status: " &
              Peer.To_String);
      else
         System_Messages.Notify
           (Error, "My_Callbacks.Peer_Status: No state information supplied");
         raise Parser.BAD_PACKET_FORMAT;
      end if;

      --  Update the peer
      AMI.Peer.List.Put (Peer => Peer);

   end Peer_Status;

   --------------
   --  Unlink  --
   --------------

--     procedure Unlink (Packet : in Parser.Packet_Type) is
--        ID1, ID2 : Call_ID.Call_ID_Type := Null_Call_ID;
--     begin
--        ID1 := Call_ID.Create
--          (To_String (Packet.Get_Value (Parser.UniqueID1)));
--        ID2 := Call_ID.Create
--          (To_String (Packet.Get_Value (Parser.UniqueID2)));
--        Calls.List.Unlink (ID1 => ID1,
--                         ID2 => ID2);
--     end Unlink;

   procedure Queue_Abandon (Packet : in Parser.Packet_Type) is
      Context           : constant String := Package_Name & ".Queue_Abandon";
--      Call              : Call_Type := Null_Call;
      Queue             : Unbounded_String := Null_Unbounded_String;
      Position          : Integer := -1;
      Original_Position : Integer := -1;
      Hold_Time         : Integer := -1;
      Unique_ID         : String renames Packet.Get_Value (Parser.UniqueID);
   begin
--        Call.ID := Create
--          (To_String (Packet.Get_Value (Parser.UniqueID)));

      Position := Integer'Value
        (To_String (Packet.Get_Value (Parser.Position)));

      Queue := Packet.Get_Value (Parser.Queue);

      Original_Position := Integer'Value
        (To_String (Packet.Get_Value (Parser.OriginalPosition)));

      Hold_Time := Integer'Value
        (To_String (Packet.Get_Value (Parser.HoldTime)));

      System_Messages.Notify (Debug, Context & ": Channel with ID" &
                                Unique_ID & " left queue " &
                                To_String (Queue) & " after" & Hold_Time'Img &
                                " seconds. Position:" & Position'Img & "," &
                                " original position" & Original_Position'Img);
   end Queue_Abandon;

   procedure Register_Handlers is
   begin
      AMI.Observers.Register (Event   => AMI.Event.CoreShowChannel,
                              Handler => Core_Show_Channel'Access);
      AMI.Observers.Register (Event   => AMI.Event.CoreShowChannelsComplete,
                              Handler => Core_Show_Channels_Complete'Access);
      AMI.Observers.Register (Event   => AMI.Event.PeerStatus,
                              Handler => Peer_Status'Access);
      AMI.Observers.Register (Event   => AMI.Event.PeerEntry,
                              Handler => Peer_Entry'Access);
      AMI.Observers.Register (Event   => AMI.Event.PeerlistComplete,
                              Handler => Peer_List_Complete'Access);
      AMI.Observers.Register (Event   => AMI.Event.QueueCallerAbandon,
                              Handler => Queue_Abandon'Access);
   end Register_Handlers;
begin
   Register_Handlers;

end PBX.Event_Handlers;
