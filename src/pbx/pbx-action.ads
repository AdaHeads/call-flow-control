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

--  This is our PBX-boundry layer that (hopefully)

with AMI.Packet.Action;
with AMI.Parser;

with PBX.Call;
with Model.Agent;

package PBX.Action is
   use PBX;

   --  TODO add a cleanup task or housekeeping hook.

   Timeout   : exception;
   Error     : exception;

   function Value (ID : AMI.Action_ID_Type)
                  return Reply_Ticket;

   type Response_Handler is new AMI.Packet.Action.Response_Handler_Type;
   subtype Response is AMI.Parser.Packet_Type;

   Ignore : constant Response_Handler;
   --  Standard null-body handler for ignoring responses.

   function Bridge (Source      : in PBX.Call.Channel_Identification;
                    Destination : in PBX.Call.Channel_Identification;
                    On_Response : in Response_Handler := Ignore)
                    return Reply_Ticket;
   --  Bridges two channels. The caller must explicitly specify which legs are
   --  bridged. Raises Null_Channel if either channel is null.

   function Hangup (ID : in Call.Identification) return Reply_Ticket;
   --  Request a hangup on a given call.

   function Login (Username    : in String;
                   Secret      : in String;
                   On_Response : in Response_Handler :=
                     Ignore) return Reply_Ticket;

   function Logoff (On_Response : in Response_Handler :=
                      Ignore) return Reply_Ticket;

   function List_Channels (On_Response : in Response_Handler :=
                             Ignore) return Reply_Ticket;

   function List_SIP_Peers (On_Response : in Response_Handler :=
                              Ignore) return Reply_Ticket;

   function Originate (Agent       : in Model.Agent.Agent_Type;
                       Extension   : in String)
                       return Reply_Ticket;

   procedure Originate (Agent       : in Model.Agent.Agent_Type;
                        Extension   : in String);
   --  Synchronous originate. Raises Timeout or Error when either occurs.
   --  When successful it allocated a call within the call list and
   --  Creates a link between the ticket and the allocated call.
   --  It can later be retrieved by calling "Origination_Request".

   function Origination_Request (Ticket : in Reply_Ticket)
                                 return Call.Identification;
   --  Breaks and returns the link between a reply ticket and an allocated
   --  call. Raises Not_Found when not link is present.

   function Park (ID               : in Call.Identification;
                  Parking_Lot      : in String := "";
                  On_Response      : in Response_Handler := Ignore)
                  return Reply_Ticket;

   function Redirect
     (Channel      : in String;
      Extension    : in String;
      Context      : in String;
      On_Response  : in Response_Handler := Ignore) return Reply_Ticket;

   procedure Wait_For (Ticket : in Reply_Ticket);
   --  Blocking call that waits until a reply for an action is received.

   function Wait_For (Ticket : in Reply_Ticket) return Response;
private
   Ignore : constant Response_Handler :=
              AMI.Packet.Action.Null_Reponse_Handler'Access;

end PBX.Action;
