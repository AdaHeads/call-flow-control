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

with AMI.Channel_ID;
with AMI.Observers;
with AMI.Event;
with AMI.Parser;
with AMI.Trace;
with View.Call;

with Handlers.Notifications;
with Client_Notification.Queue;

package body PBX.Call.Event_Handers is
   use AMI.Trace;

   package Notification renames Handlers.Notifications;

   procedure Dial (Packet : in Parser.Packet_Type);

   procedure Join (Packet : in Parser.Packet_Type);

      ------------
   --  Dial  --
   ------------

   procedure Dial (Packet : in Parser.Packet_Type) is
      Context   : constant String := Package_Name & ".Dial";

      Sub_Event   : String renames Packet.Get_Value (Parser.SubEvent);
      Channel     : String renames Packet.Get_Value (Parser.Channel);
      Destination : String renames Packet.Get_Value (Parser.Destination);
   begin
      --  There is a sequence to a Dial event represented by a SubEvent.
      --  It consists of "Begin" or "End"
      if Sub_Event = "Begin" then

         if AMI.Channel_ID.Value (Channel).Is_Local then
            Create_And_Insert
              (Inbound        => False,
               Channel        => Channel,
               State          => Dialing,
               B_Leg          => Destination);
         else
            --  The call should already exist, just update the B_Leg and
            --  set state to dialing

            Get (Channel => Value (Channel)).Dial (Value (Destination));

         end if;

         AMI.Trace.Log (Debug, "Begin: "&  View.Call.To_JSON
                        (Get (Channel => Value (Channel))).Write);

      --  When a Dial event ends, the call is over, and must thus be removed
      --  From the call list.
      elsif Sub_Event = "End" then
         Get (Channel => Value (Channel)).End_Dial;

         AMI.Trace.Log (Debug, "End: " & View.Call.To_JSON
                        (Get (Channel => Value (Channel))).Write);
      else
         AMI.Trace.Log
           (Error, Package_Name & "." & Context & ": " &
              "unknown SubEvent: " & Sub_Event);
      end if;
   end Dial;

   procedure Join (Packet : in Parser.Packet_Type) is
      Context   : constant String := Package_Name & ".Join";
      Channel     : String renames Packet.Get_Value (Parser.Channel);
   begin
      Create_And_Insert
        (Inbound        => True,
         Channel        => Channel,
         State          => Queued);
--      Get (Channel => Value (Channel)).Enqueue;

      Notification.Broadcast
        (Client_Notification.Queue.Join
           (Get (Channel => Value (Channel))).To_JSON);
   exception
         when others =>
         AMI.Trace.Log
           (Error, Context & ": Got a Join event, " &
              "on a channel there is not in the channel list. " &
              "Channel: " & Channel);
   end Join;

   procedure Register_Handlers is
   begin
      AMI.Observers.Register (Event   => AMI.Event.Dial,
                              Handler => Dial'Access);
      AMI.Observers.Register (Event   => AMI.Event.Join,
                              Handler => Join'Access);
   end Register_Handlers;

begin
   Register_Handlers;
end PBX.Call.Event_Handers;
