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

with Ada.Calendar;
with Ada.Strings.Unbounded;
with Common;

with AMI.Trace;

package body AMI.Response is
   use AMI;

   -------------
   --  Claim  --
   -------------

   function Claim (Ticket : in Action_ID_Type;
                      Timeout   : in Duration := 3.0)
                   return Parser.Packet_Type is
      Packet : Parser.Packet_Type := Parser.New_Packet;
   begin
      Wait_For (Ticket, Timeout);

      Responses.Pop (Ticket, Packet);

      return Packet;
   end Claim;

   ------------
   --  Hash  --
   ------------

   function Hash (Key : in Action_ID_Type)
                           return Ada.Containers.Hash_Type is
   begin
      return Action_ID_Type'Pos (Key);
   end Hash;

   ----------------------------
   --  Hash_Equivalent_Keys  --
   ----------------------------

   function Hash_Equivalent_Keys (Left, Right : in Action_ID_Type)
                                  return Boolean is
   begin
      return Left = Right;
   end Hash_Equivalent_Keys;

   --------------
   --  Notify  --
   --------------

   procedure Notify (Packet : in AMI.Parser.Packet_Type) is
      use Ada.Strings.Unbounded;
      Context  : constant String := Package_Name & ".Notify";
      Key      : Action_ID_Type  := Null_Action_ID;
   begin

      Key := Action_ID_Type'Value (Packet.Get_Value (AMI.Parser.ActionID));
      AMI.Trace.Debug (Message => "Dispatching" & Key'Img,
                       Context => Context,
                       Level   => 20);

      Responses.Buffer_Packet (Ticket => Packet.Action_ID,
                               Packet => Packet);
   exception
      when Constraint_Error =>
         AMI.Trace.Error (Message => "AMI.Response.Notify: No "
                          & " subscribed response found for ID : " & Key'Img,
                          Context  => Context);
   end Notify;

   -----------------------
   --  Packet_Lifetime  --
   -----------------------

   procedure Packet_Lifetime (Lifetime : in Duration) is
   begin
      Current_Packet_Lifetime := Lifetime;
   end Packet_Lifetime;

   ------------------
   --  Set_Packet  --
   ------------------

   procedure Set_Packet (Reponse :    out Stored_Response;
                     Packet  : in     AMI.Parser.Packet_Type) is
   begin
      Reponse.Packet := Packet;
   end Set_Packet;

   -----------------
   --  Subscribe  --
   -----------------

   procedure Subscribe (Reply_For : in Request) is
      Context  : constant String := Package_Name & ".Subscribe_Handler";
   begin
      AMI.Trace.Debug (Message => "subscribing" & Reply_For.Action_ID'Img,
                       Context => Context,
                       Level   => 20);

      Responses.Subscribe (Ticket  => Reply_For.Action_ID,
                           Handler => Reply_For.Response_Handler);
   end Subscribe;

   -----------------
   --  Subscribe  --
   -----------------

   procedure Subscribe (Ticket : in Action_ID_Type) is
      Context : constant String := Package_Name & ".Subscribe_Response";
   begin
      AMI.Trace.Debug (Message => "subscribing" & Ticket'Img,
                       Context => Context,
                       Level   => 20);
      Responses.Subscribe (Ticket);
   end Subscribe;

   ----------------
   --  Wait_For  --
   ----------------

   procedure Wait_For (Ticket  : in Action_ID_Type;
                       Timeout : in Duration := 3.0) is
      use Common;
      use type Ada.Calendar.Time;

      Context : constant String := Package_Name & ".Wait_For";

      Deadline : constant Ada.Calendar.Time := Current_Time + Timeout;
   begin
      AMI.Trace.Debug (Message => "Waiting",
                       Context => Context);
      loop
         exit when Current_Time > Deadline;
         if Responses.Reply_Received (Ticket) then
            AMI.Trace.Debug (Message => "Got reply",
                             Context => Context);
            return;
         end if;

         delay 0.1;
      end loop;
      AMI.Trace.Debug (Message => "No reply received",
                       Context => Context);

      raise AMI.Response.Timeout;
   end Wait_For;

   protected body Responses is

      procedure Buffer_Packet (Ticket : in Action_ID_Type;
                               Packet : in AMI.Parser.Packet_Type) is

         procedure Set_Packet (Key  : in     Action_ID_Type;
                                      Item : in out Stored_Response);
         --  TODO.

         ------------------
         --  Set_Packet  --
         ------------------

         procedure Set_Packet (Key  : in     Action_ID_Type;
                               Item : in out Stored_Response) is
            pragma Unreferenced (Key);
         begin
            Item.Packet    := Packet;
            Item.Got_Reply := True;
         end Set_Packet;
      begin
         --  For some reason, AMI seems to send duplicate responses to
         --  requests. We simpy ignore further responses.
         if not Response_List.Element (Ticket).Got_Reply then
            Response_List.Update_Element
              (Position => Response_List.Find (Ticket),
               Process  => Set_Packet'Access);
         else
            AMI.Trace.Error
              (Message => "Got second (or more) reply to request - ignoring",
               Context => "Buffer_Packet");
         end if;
      end Buffer_Packet;

      -----------
      --  Pop  --
      -----------

      procedure Pop (Ticket : in      Action_ID_Type;
                     Packet :     out Parser.Packet_Type) is
         procedure Remove_Subscriber (Key  : in     Action_ID_Type;
                                      Item : in out Stored_Response);
         --  Decrement the number of subscribers.

         -------------------------
         --  Remove_Subscriber  --
         -------------------------

         procedure Remove_Subscriber (Key  : in     Action_ID_Type;
                                      Item : in out Stored_Response) is
            pragma Unreferenced (Key);
         begin
            Item.Subscribers := Item.Subscribers - 1;
         end Remove_Subscriber;

      begin
         --  Extract the packet.
         Packet := Response_List.Element (Ticket).Packet;

         --  Decrement the number of subscribers.
         Response_List.Update_Element
           (Position => Response_List.Find (Ticket),
            Process  => Remove_Subscriber'Access);

         AMI.Trace.Debug (Message => "Subscriptions:" &
                            Response_List.Element (Ticket).Subscribers'Img,
                          Context => "Pop");

         --  If there are no more subscribers, release the resources.
         if Response_List.Element (Ticket).Subscribers = 0 then
            Response_List.Delete (Ticket);
            AMI.Trace.Debug (Message => "Deleting ticket",
                          Context => "Pop");
         end if;

      end Pop;

      ----------------------
      --  Reply_Received  --
      ----------------------

      function Reply_Received (Ticket : in Action_ID_Type) return Boolean is
      begin
         return Response_List.Element (Ticket).Got_Reply;
      end Reply_Received;

      -----------------
      --  Subscribe  --
      -----------------

      procedure Subscribe
        (Ticket  : in Action_ID_Type;
         Handler : in Packet.Action.Response_Handler_Type := Ignore) is

         procedure Add_Subscriber (Key  : in     Action_ID_Type;
                                   Item : in out Stored_Response);
         --  Increment the number of subscribers.

         procedure Set_Handler (Key  : in     Action_ID_Type;
                                Item : in out Stored_Response);
         --  Replace the handler currently associated with the ticket.

         ----------------------
         --  Add_Subscriber  --
         ----------------------

         procedure Add_Subscriber (Key  : in     Action_ID_Type;
                                   Item : in out Stored_Response) is
            pragma Unreferenced (Key);
         begin
            Item.Subscribers := Item.Subscribers + 1;
         end Add_Subscriber;

         -------------------
         --  Set_Handler  --
         -------------------

         procedure Set_Handler (Key  : in     Action_ID_Type;
                                Item : in out Stored_Response) is
            pragma Unreferenced (Key);
         begin
            Item.Response_Handler := Handler;
         end Set_Handler;

      begin
         AMI.Trace.Debug (Message => "Subscribing" & Ticket'Img,
                          Context => "Subscribe");
         if Response_List.Contains (Ticket) then
            AMI.Trace.Debug (Message => "Found in list" & Ticket'Img,
                             Context => "Subscribe");
            if Handler /= Ignore then
               Response_List.Update_Element
                 (Position => Response_List.Find (Ticket),
                  Process  => Set_Handler'Access);
            end if;

            Response_List.Update_Element
              (Position => Response_List.Find (Ticket),
               Process  => Add_Subscriber'Access);

         else
            AMI.Trace.Debug (Message => "Not found in list, inserting"
                             & Ticket'Img,
                             Context => "Subscribe");
            Response_List.Insert (Ticket,
                                  (Got_Reply        => False,
                                   Subscribers      => 1,
                                   Response_Handler => Handler,
                                   Packet           => Parser.New_Packet));
         end if;
         AMI.Trace.Debug (Message => "Subscriptions:" &
                            Response_List.Element (Ticket).Subscribers'Img,
                          Context => "Subscribe");
      end Subscribe;

   end Responses;

end AMI.Response;
