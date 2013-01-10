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

with AMI.Packet.Action;

with PBX.Call;

package PBX.Action is

   Timeout : exception;

   type Response_Handler is new AMI.Packet.Action.Response_Handler_Type;

   Ignore : constant Response_Handler;

   function Bridge (Channel1    : in String;
                    Channel2    : in String;
                    On_Response : in Response_Handler := Ignore)
                  return Reply_Ticket;

   function Hangup (ID : in PBX.Call.Identification) return Reply_Ticket;

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

   function Originate (Peer        : in String;
                       Context     : in String;
                       Extension   : in String;
                       On_Response : in Response_Handler := Ignore)
                       return Reply_Ticket;

   function Park (Channel          : in String;
                  Fallback_Channel : in String;
                  Parking_Lot      : in String := "";
                  On_Response      : in Response_Handler := Ignore)
                  return Reply_Ticket;

   function Redirect
     (Channel      : in String;
      Extension    : in String;
      Context      : in String;
      On_Response  : in Response_Handler := Ignore) return Reply_Ticket;

   procedure Wait_For (Ticket : in Reply_Ticket);
private
   Ignore : constant Response_Handler :=
               AMI.Packet.Action.Null_Reponse_Handler'Access;

end PBX.Action;
