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

with AMI.Parser;
with AMI.Packet.Action;
package AMI.Response is
   use AMI.Packet.Action;

   Package_Name : constant String := "AMI.Response";

   Timeout : exception;

   procedure Subscribe (Reply_For : in Request);
   --  Subscribe for a reply on the given request. Upon reception of reply
   --  The response handler will be invoked with the packet as parameter.

   procedure Subscribe (Ticket : in Action_ID_Type);
   --  Subscribe for a reply with specifed ticket.
   --  The packet will be stored temporarily in an internal buffer, enabling
   --  us to pick it up later. Thus, Claim must be called on the same ticket
   --  To return it. Non-claimed packets are cleaned up when the housekeeping
   --  procedure is called.

   procedure Wait_For (Ticket  : in Action_ID_Type;
                       Timeout : in Duration := 3.0);
   --  Provides an explicit synchonization mechanism. Caller will block until
   --  a reply is received before timout or raise Timeout.

   function Claim (Ticket  : in Action_ID_Type;
                   Timeout : in Duration := 3.0)
                      return AMI.Parser.Packet_Type;
   --  Provides an explicit synchonization mechanism
   --  that returns the packet upon completion.

   procedure Notify (Packet : in AMI.Parser.Packet_Type);
   --  Notify subscribers about a reponse.

   procedure Housekeeping is null;
   --  Outlined. Implement it!

   procedure Packet_Lifetime (Lifetime : in Duration);
   --  How long should the packets live in the internal buffer before
   --  being marked for removal and cleaned by Housekeeping.
   --  Default is 10 seconds.
private

   Current_Packet_Lifetime : Duration := 10.0;

   type Stored_Response is tagged
      record
         Got_Reply        : Boolean := False;
         Subscribers      : Natural := 0;
         Packet           : AMI.Parser.Packet_Type;
         Response_Handler : AMI.Packet.Action.Response_Handler_Type;
      end record;
   procedure Set_Packet (Reponse :    out Stored_Response;
                         Packet  : in     AMI.Parser.Packet_Type);

   function Hash (Key : in Action_ID_Type)
                           return Ada.Containers.Hash_Type;
   function Hash_Equivalent_Keys
     (Left, Right : in Action_ID_Type)
      return Boolean;

   package Response_Storage is new Ada.Containers.Hashed_Maps
     (Key_Type        => Action_ID_Type,
      Element_Type    => Stored_Response,
      Hash            => Hash,
      Equivalent_Keys => Hash_Equivalent_Keys);

   Empty_Response : constant Stored_Response :=
                      (Got_Reply        => False,
                       Subscribers      => 0,
                       Packet           => AMI.Parser.New_Packet,
                       Response_Handler =>
                         AMI.Packet.Action.Null_Reponse_Handler'Access);

   protected Responses is
      procedure Subscribe
        (Ticket  : in Action_ID_Type;
         Handler : in Packet.Action.Response_Handler_Type := Ignore);
      procedure Buffer_Packet (Ticket : in Action_ID_Type;
                               Packet : in AMI.Parser.Packet_Type);

      function Reply_Received (Ticket : in Action_ID_Type) return Boolean;
      procedure Pop (Ticket : in      Action_ID_Type;
                     Packet :     out Parser.Packet_Type);

   private
      Response_List : Response_Storage.Map := Response_Storage.Empty_Map;
   end Responses;

end AMI.Response;
