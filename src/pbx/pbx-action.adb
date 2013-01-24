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

with Ada.Containers.Hashed_Maps;

with AMI.Response;
with AMI.Trace;

package body PBX.Action is
   use type PBX.Call.Identification;

   function Remote_End (ID : in Call.Identification) return String;
   --  Determines which end of a call is the remote end.

   function Local_End (ID : in Call.Identification) return String;
   --  Determines which end of a call is the local end.

   function Value (Handler : Response_Handler)
                  return AMI.Packet.Action.Response_Handler_Type;
   --  Cast function.

   function Hash (Item : in PBX.Reply_Ticket)
                  return Ada.Containers.Hash_Type;
   --  For origination requests.

   function Equivalent_Keys (Left, Right : in PBX.Reply_Ticket)
                             return Boolean;
   --  For origination requests.

   package Origination_Request_Storage is new
     Ada.Containers.Hashed_Maps
       (Key_Type        => PBX.Reply_Ticket,
        Element_Type    => PBX.Call.Identification,
        Hash            => Hash,
        Equivalent_Keys => Equivalent_Keys);
   --  Storage container for origination requests.

   protected Origination_Requests is
      procedure Link (Ticket  : in Reply_Ticket;
                      Call_ID : in Call.Identification);

      procedure Unlink (Ticket  : in Reply_Ticket);

      procedure Unlink (Ticket  : in     Reply_Ticket;
                        Call_ID :    out Call.Identification);
   private
      Origination_List : Origination_Request_Storage.Map :=
                           Origination_Request_Storage.Empty_Map;
   end Origination_Requests;
   --  Origination requests a temporary storage that keeps track on which
   --  calls the agents have originated, and links them with their
   --  corresponding Action_ID/Ticket.

   ----------------------------
   --  Origination_Requests  --
   ----------------------------

   protected body Origination_Requests is

      ------------
      --  Link  --
      ------------

      procedure Link (Ticket  : in Reply_Ticket;
                      Call_ID : in Call.Identification) is
      begin
         Origination_List.Insert
           (Key      => Ticket,
            New_Item => Call_ID);
      end Link;

      --------------
      --  Unlink  --
      --------------

      procedure Unlink (Ticket  : in     Reply_Ticket;
                        Call_ID :    out Call.Identification) is
      begin
         Call_ID := Call.Get (Origination_List.Element (Ticket)).ID;
         Origination_List.Delete (Ticket);
      end Unlink;

      --------------
      --  Unlink  --
      --------------

      procedure Unlink (Ticket : in Reply_Ticket) is
         Dummy_ID : Call.Identification;
         pragma Unreferenced (Dummy_ID);
      begin
         Dummy_ID := Call.Remove (Origination_List.Element (Ticket)).ID;
         Origination_List.Delete (Ticket);
      end Unlink;

   end Origination_Requests;

   --------------
   --  Bridge  --
   --------------

   function Bridge (Source      : in PBX.Call.Channel_Identification;
                    Destination : in PBX.Call.Channel_Identification;
                    On_Response : in Response_Handler := Ignore)
                  return Reply_Ticket
   is
      use PBX.Call;
      Bridge_Action : AMI.Packet.Action.Request :=
                        AMI.Packet.Action.Bridge
                          (Channel1    => To_String (Source),
                           Channel2    => To_String (Destination),
                           On_Response => Value (On_Response));
   begin
      if
        Source      = Call.Null_Channel_Identification or
        Destination = Call.Null_Channel_Identification
      then
         raise Call.Null_Channel with
           "Source "     & To_String (Source) & " " &
           "Destination" & To_String (Destination);
      end if;

      PBX.Client.Send (Bridge_Action.To_AMI_Packet);

      return Value (Bridge_Action.Action_ID);
   end Bridge;

   -----------------------
   --  Equivalent_Keys  --
   -----------------------

   function Equivalent_Keys (Left, Right : in PBX.Reply_Ticket)
                             return Boolean is
   begin
      return Left = Right;
   end Equivalent_Keys;

   ------------
   -- Hangup --
   ------------

   function Hangup (ID : in PBX.Call.Identification) return Reply_Ticket is
      use PBX.Call;

      Hangup_Action : AMI.Packet.Action.Request :=
                        AMI.Packet.Action.Hangup
                          (Channel => To_String (Get (ID).Channel));
   begin
      if ID /= Null_Identification then
         PBX.Client.Send (Hangup_Action.To_AMI_Packet);

         return Value (Hangup_Action.Action_ID);
      end if;

      return Null_Reply;

   end Hangup;

   ------------
   --  Hash  --
   ------------

   function Hash (Item : in PBX.Reply_Ticket)
                  return Ada.Containers.Hash_Type is
   begin
      return PBX.Reply_Ticket'Pos (Item);
   end Hash;

   -------------------
   -- List_Channels --
   -------------------

   function List_Channels (On_Response : in Response_Handler :=
                             Ignore) return Reply_Ticket
   is
      List_Channels_Action  : AMI.Packet.Action.Request :=
                                AMI.Packet.Action.Core_Show_Channels
                                  (On_Response => Value (On_Response));
   begin
      Client.Send (List_Channels_Action);

      return Value (List_Channels_Action.Action_ID);
   end List_Channels;

   ---------------------
   --  List_SIP_Peers --
   ---------------------

   function List_SIP_Peers (On_Response : in Response_Handler :=
                               Ignore) return Reply_Ticket is
      List_Peers_Action : AMI.Packet.Action.Request :=
                            AMI.Packet.Action.SIP_Peers
                              (On_Response => Value (On_Response));
   begin
      Client.Send (List_Peers_Action);

      return Value (List_Peers_Action.Action_ID);
   end List_SIP_Peers;

   -----------------
   --  Local_End  --
   -----------------

   function Local_End (ID : in Call.Identification) return String is
      use PBX.Call;
   begin
      if Call.Get (ID).Inbound then
         return To_String (Call.Get (ID).B_Leg);
      else
         return To_String (Call.Get (ID).Channel);
      end if;
   end Local_End;

   -----------
   -- Login --
   -----------

   function Login (Username    : in String;
                   Secret      : in String;
                   On_Response : in Response_Handler :=
                     Ignore) return Reply_Ticket is
      Login_Action : AMI.Packet.Action.Request :=
                       AMI.Packet.Action.Login
                         (Username    => Username,
                          Secret      => Secret,
                          On_Response => Value (On_Response));
   begin
      Client.Send (Login_Action);
      return Value (Login_Action.Action_ID);
   end Login;

   ------------
   -- Logoff --
   ------------

   function Logoff (On_Response : in Response_Handler :=
                      Ignore) return Reply_Ticket is
      List_Peers_Action : AMI.Packet.Action.Request :=
                            AMI.Packet.Action.Logoff
                              (On_Response => Value (On_Response));
   begin
      Client.Send (List_Peers_Action);

      return Value (List_Peers_Action.Action_ID);
   end Logoff;

   -----------------
   --  Originate  --
   -----------------

   procedure Originate (Agent       : in Model.Agent.Agent_Type;
                        Extension   : in String) is
      Context          : constant String := Package_Name & ".Originate";
      Originate_Action : AMI.Packet.Action.Request :=
                        AMI.Packet.Action.Originate
                             (Channel     => Agent.Peer_ID.To_String,
                              Extension   => Extension,
                              Context     => Agent.Context,
                              Priority    => 1,
                              On_Response => Value (Ignore));
      Packet           : AMI.Parser.Packet_Type;
   begin
      --  Outline the call. This is done prior to sending the action to assure
      --  that a request exist in the list when the action completes, thus
      --  avoiding the race condition.
      Origination_Requests.Link
        (Ticket  => Value (Originate_Action.Action_ID),
         Call_ID => PBX.Call.Allocate (Assigned_To => Agent.ID));

      AMI.Trace.Debug
        (Message => "Sending Originate request with ticket " &
          Originate_Action.Action_ID'Img,
         Context => Context,
         Level   => 1);
      Packet := Client.Send (Originate_Action);

      if Packet.Header_Value /= "Success" then
         Origination_Requests.Unlink (Value (Originate_Action.Action_ID));
         --  Remove the allocated call if the request fails.
         raise Error with Packet.Get_Value (AMI.Parser.Message);
      end if;
   end Originate;

   -----------------
   --  Originate  --
   -----------------

   function Originate (Agent       : in Model.Agent.Agent_Type;
                       Extension   : in String)
                       return Reply_Ticket
   is
      Originate_Action : AMI.Packet.Action.Request :=
                            AMI.Packet.Action.Originate
                             (Channel     => Agent.Peer_ID.To_String,
                              Extension   => Extension,
                              Context     => Agent.Context,
                              Priority    => 1,
                              On_Response => Value (Ignore));
   begin
      Client.Send (Originate_Action);

      return Value (Originate_Action.Action_ID);
   end Originate;

   ---------------------------
   --  Origination_Request  --
   ---------------------------

   function Origination_Request (Ticket : in Reply_Ticket)
                                 return Call.Identification is
      Context : constant String := Package_Name & ".Origination_Request";
      ID : Call.Identification := Call.Null_Identification;
   begin
      Origination_Requests.Unlink (Ticket  => Ticket,
                                   Call_ID => ID);

      return ID;
   exception
      when Constraint_Error =>
         AMI.Trace.Error (Message => "No request found for origination with " &
                            "ID" & Ticket'Img,
                          Context => Context);
         raise Constraint_Error with "No request found for origination with " &
                            "ID" & Ticket'Img;
   end Origination_Request;

   ----------
   -- Park --
   ----------

   function Park (ID               : in Call.Identification;
                  Parking_Lot      : in String := "";
                  On_Response      : in Response_Handler := Ignore)
                  return Reply_Ticket is
      use PBX.Call;
      Park_Action : AMI.Packet.Action.Request :=
                      AMI.Packet.Action.Park
                        (Channel     => Remote_End (ID),
                         Channel2    => Local_End (ID),
                         Timeout     => 7200.0,
                         Parkinglot  => Parking_Lot,
                         On_Response =>
                           Value (On_Response));
   begin
      Client.Send (Park_Action);

      return Value (Park_Action.Action_ID);
   end Park;

   --------------
   -- Redirect --
   --------------

   function Redirect
     (Channel      : in String;
      Extension    : in String;
      Context      : in String;
      On_Response  : in Response_Handler := Ignore) return Reply_Ticket
   is
      Redirect_Action : AMI.Packet.Action.Request :=
                          AMI.Packet.Action.Redirect
                            (Channel      => Channel,
                             Extension    => Extension,
                             Context      => Context,
                             On_Response  => Value (On_Response));
   begin
      Client.Send (Redirect_Action);

      return Value (Redirect_Action.Action_ID);
   end Redirect;

   ------------------
   --  Remote_End  --
   ------------------

   function Remote_End (ID : in Call.Identification) return String is
      use PBX.Call;
   begin
      if Call.Get (ID).Inbound then
         return To_String (Call.Get (ID).Channel);
      else
         return To_String (Call.Get (ID).B_Leg);
      end if;
   end Remote_End;

   ---------------------
   -- Value functions --
   ---------------------

   function Value (Handler : Response_Handler)
                  return AMI.Packet.Action.Response_Handler_Type is
   begin
      return AMI.Packet.Action.Response_Handler_Type (Handler);
   end Value;

   function Value (ID : AMI.Action_ID_Type)
                  return Reply_Ticket is
   begin
      return Reply_Ticket (ID);
   end Value;

   --------------
   -- Wait_For --
   --------------

   procedure Wait_For (Ticket : in Reply_Ticket) is
   begin
      AMI.Response.Wait_For (Ticket => Action_ID_Type (Ticket));
   end Wait_For;

   function Wait_For (Ticket : in Reply_Ticket) return Response is
   begin
      return AMI.Response.Claim (Ticket => Action_ID_Type (Ticket));
   end Wait_For;

end PBX.Action;
