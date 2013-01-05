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

with AMI.Parser;
with AMI.Event;
with AMI.Observers;

with Model.Call;
with Model.Calls;

with System_Messages; use System_Messages;

package body AMI.Channel.Event_Handlers is
   use AMI;

   procedure New_Caller_ID (Packet : in Parser.Packet_Type);
   --  Occurs whenever a channel changes it's Caller_ID.

   procedure New_State (Packet : in Parser.Packet_Type);
   --  Update the state of a channel.

   procedure Attach_Variable (Packet : in Parser.Packet_Type);
   --  Adds a variable to a given channel.

   procedure New_Channel (Packet : in Parser.Packet_Type);
   --  A Newchannel event represents any channel created within asterisk.
   --  We collect every channel into a channel list and distribute them
   --  from there to either a call list or a peer channel list.

   -----------------------
   --  Attach_Variable  --
   -----------------------

   procedure Attach_Variable (Packet : in Parser.Packet_Type) is
      Context        : constant String  := Package_Name & ".Attach_Variable";
      Channel_Key    : US.Unbounded_String renames
                         Packet.Get_Value (Parser.Channel);
      Target_Channel : Channel.Instance := Channel.Empty_Object;
   begin
      Target_Channel := Channel.List.Get (Channel_Key);
      Target_Channel.Add_Variable (Packet.Get_Value (Parser.Variable),
                                   Packet.Get_Value (Parser.Value));
      Channel.List.Update (Key  => Channel_Key,
                           Item => Target_Channel);
      System_Messages.Notify (Debug, Context & ": Adding [" &
                                Packet.Get_Value (Parser.Variable) & "] => [" &
                                Packet.Get_Value (Parser.Value)    & "]" &
                                " to channel " & US.To_String (Channel_Key));
   exception
      when others =>
         System_Messages.Notify
           (Error, Context & ": failed to add [" &
              Packet.Get_Value (Parser.Variable) & "] => [" &
              Packet.Get_Value (Parser.Value)    & "]" &
              " to channel " & US.To_String (Channel_Key));
         raise;
   end Attach_Variable;

   ---------------------
   --  New_Caller_ID  --
   ---------------------

   procedure New_Caller_ID (Packet : in Parser.Packet_Type) is
      Context        : constant String  := "New_Caller_ID";
      Target_Channel : Channel.Instance := Channel.Empty_Object;
      Target_Call    : Call.Call_Type   := Call.Null_Call;
      Old_ID         : Call_ID.Call_ID_Type := Call_ID.Null_Call_ID;
   begin
      Target_Channel := Channel.List.Get
        (Packet.Get_Value (Parser.Channel));

      System_Messages.Notify
        (Debug, Package_Name & "." & Context & ": Changing call id from " &
           Target_Channel.Unique_ID.To_String & " to " &
           Packet.Get_Value (Parser.UniqueID));

      Target_Call := Calls.List.Get (Target_Channel.Unique_ID);
      Old_ID := Target_Call.ID;
      Target_Call.ID :=
        Call_ID.Create (Packet.Get_Value (Parser.UniqueID));

      Calls.List.Insert (Target_Call);
      Calls.List.Remove (Old_ID);
      --  Setting the new uniqueID
      Target_Channel.Unique_ID :=
        Call_ID.Create (Packet.Get_Value (Parser.UniqueID));

      Channel.List.Update (Packet.Get_Value (Parser.UniqueID),
                             Target_Channel);

   end New_Caller_ID;

   -------------------
   --  New_Channel  --
   -------------------

   procedure New_Channel (Packet : in Parser.Packet_Type) is
      use type Channel.Instance;
      Context     : constant String  := "New_Channel";
      New_Channel : constant AMI.Channel.Instance :=
                      AMI.Channel.Create (Packet => Packet);
   begin
      System_Messages.Notify
        (Debug, Package_Name & "." & Context & ": " & New_Channel.To_String);
      if not New_Channel.Is_Null then
         System_Messages.Notify
           (Debug, Package_Name & ".New_Channel: Inserting " &
              New_Channel.To_String);
         Channel.List.Insert
           (Packet.Get_Value (Parser.Channel),
            New_Channel);
      end if;
   end New_Channel;

   -----------------
   --  New_State  --
   -----------------

   procedure New_State (Packet : in Parser.Packet_Type) is
      Context           : constant String  := "New_State";
      Channel_To_Change : Channel.Instance := Channel.Empty_Object;
   begin

      System_Messages.Notify
        (Debug, Package_Name & "." & Context & ": updating channel "
         & Packet.Get_Value (Parser.Channel));
      Channel_To_Change := Channel.List.Get (Packet.Get_Value
                                             (Parser.Channel));
      Channel_To_Change.Change_State (Packet);
      Channel.List.Update (Packet.Get_Value (Parser.Channel),
                           Channel_To_Change);

   exception
      when others =>
         System_Messages.Notify (Error, Package_Name & "." & Context & ": " &
                                   "failed to update channel " &
                                   Channel_To_Change.To_String);
         raise;
   end New_State;

   -------------------------
   --  Register_Handlers  --
   -------------------------

   procedure Register_Handlers is
   begin
      AMI.Observers.Register
        (Event   => AMI.Event.Newcallerid,
         Handler => New_Caller_ID'Access);

      AMI.Observers.Register
        (Event   => AMI.Event.VarSet,
         Handler => Attach_Variable'Access);

      AMI.Observers.Register
        (Event   => AMI.Event.Newchannel,
         Handler => New_Channel'Access);

      AMI.Observers.Register
        (Event   => AMI.Event.Newstate,
         Handler => New_State'Access);
   end Register_Handlers;

end AMI.Channel.Event_Handlers;
