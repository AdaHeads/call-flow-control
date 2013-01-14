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

with AMI.Response;

package body PBX.Action is

   function Cast (Handler : Response_Handler)
                  return AMI.Packet.Action.Response_Handler_Type;
   function Cast (ID : AMI.Action_ID_Type)
                  return Reply_Ticket;

   --------------
   --  Bridge  --
   --------------

   function Bridge (Source      : in PBX.Call.Identification;
                    Destination : in PBX.Call.Identification;
                    On_Response : in Response_Handler := Ignore)
                  return Reply_Ticket
   is
      use PBX.Call;
      Bridge_Action : AMI.Packet.Action.Request :=
                        AMI.Packet.Action.Bridge
                          (Channel1    => To_String (Get (Source).Channel),
                           Channel2    =>
                             To_String (Get (Destination).Channel),
                           On_Response => Cast (On_Response));
   begin
      PBX.Client.Send (Bridge_Action.To_AMI_Packet);

      return Cast (Bridge_Action.Action_ID);
   end Bridge;

   --------------------
   -- Cast functions --
   --------------------

   function Cast (Handler : Response_Handler)
                  return AMI.Packet.Action.Response_Handler_Type is
   begin
      return AMI.Packet.Action.Response_Handler_Type (Handler);
   end Cast;

   function Cast (ID : AMI.Action_ID_Type)
                  return Reply_Ticket is
   begin
      return Reply_Ticket (ID);
   end Cast;

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

         return Cast (Hangup_Action.Action_ID);
      end if;

      return Null_Reply;

   end Hangup;

   -------------------
   -- List_Channels --
   -------------------

   function List_Channels (On_Response : in Response_Handler :=
                             Ignore) return Reply_Ticket
   is
      List_Channels_Action  : AMI.Packet.Action.Request :=
                                AMI.Packet.Action.Core_Show_Channels
                                  (On_Response => Cast (On_Response));
   begin
      Client.Send (List_Channels_Action);

      return Cast (List_Channels_Action.Action_ID);
   end List_Channels;

   ---------------------
   --  List_SIP_Peers --
   ---------------------

   function List_SIP_Peers (On_Response : in Response_Handler :=
                               Ignore) return Reply_Ticket is
      List_Peers_Action : AMI.Packet.Action.Request :=
                            AMI.Packet.Action.SIP_Peers
                              (On_Response => Cast (On_Response));
   begin
      Client.Send (List_Peers_Action);

      return Cast (List_Peers_Action.Action_ID);
   end List_SIP_Peers;

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
                          On_Response => Cast (On_Response));
   begin
      Client.Send (Login_Action);
      return Cast (Login_Action.Action_ID);
   end Login;

   ------------
   -- Logoff --
   ------------

   function Logoff (On_Response : in Response_Handler :=
                      Ignore) return Reply_Ticket is
      List_Peers_Action : AMI.Packet.Action.Request :=
                            AMI.Packet.Action.Logoff
                              (On_Response => Cast (On_Response));
   begin
      Client.Send (List_Peers_Action);

      return Cast (List_Peers_Action.Action_ID);
   end Logoff;

   function Originate (Agent       : in Model.Agent.Agent_Type;
                       Extension   : in String;
                       On_Response : in Response_Handler := Ignore)
                       return Reply_Ticket
   is
      Originate_Action : AMI.Packet.Action.Request :=
                            AMI.Packet.Action.Originate
                             (Channel     => Agent.Peer_ID.To_String,
                              Extension   => Extension,
                              Context     => Agent.Context,
                              Priority    => 1,
                              On_Response => Cast (On_Response));
   begin
      Client.Send (Originate_Action);

      return Cast (Originate_Action.Action_ID);
   end Originate;

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
                        (Channel     => To_String (Call.Get (ID).Channel),
                         Channel2    => To_String (Call.Get (ID).B_Leg),
                         Timeout     => 7200.0,
                         Parkinglot  => Parking_Lot,
                         On_Response =>
                           Cast (On_Response));
   begin
      Client.Send (Park_Action);

      return Cast (Park_Action.Action_ID);
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
                             On_Response  => Cast (On_Response));
   begin
      Client.Send (Redirect_Action);

      return Cast (Redirect_Action.Action_ID);
   end Redirect;

   --------------
   -- Wait_For --
   --------------

   procedure Wait_For (Ticket : in Reply_Ticket) is
   begin
      AMI.Response.Wait_For (Action_ID => Action_ID_Type (Ticket));
   end Wait_For;

end PBX.Action;
