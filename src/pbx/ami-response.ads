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

   procedure Subscribe_Handler (Reply_For : in Request);
   --  Subscribe for a reply on the given request.

   procedure Subscribe_Response (Action_ID : in Action_ID_Type);
   --  Subscribe for a reply on the given request.

   procedure Wait_For (Action_ID : in Action_ID_Type;
                       Timeout   : in Duration := 3.0);
   --  Provides an explicit synchonization mechanism

   function Wait_For (Action_ID : in Action_ID_Type;
                      Timeout   : in Duration := 3.0)
                      return AMI.Parser.Packet_Type;
   --  Provides an explicit synchonization mechanism
   --  that returns the packet upon completion.

   procedure Notify (Packet : in AMI.Parser.Packet_Type);
   --  Notify subscribers about a reposense
private

   type Stored_Response is tagged
      record
         Subscribers : Natural := 0;
         Packet      : AMI.Parser.Packet_Type;
      end record;
   procedure Set_Packet (Reponse :    out Stored_Response;
                         Packet  : in     AMI.Parser.Packet_Type);

   function Hash_Function (Key : in Action_ID_Type)
                           return Ada.Containers.Hash_Type;
   function Hash_Equivalent_Keys
     (Left, Right : in Action_ID_Type)
      return Boolean;

   package Response_Ticket_Storage is new Ada.Containers.Hashed_Maps
     (Key_Type        => Action_ID_Type,
      Element_Type    => AMI.Packet.Action.Response_Handler_Type,
      Hash            => Hash_Function,
      Equivalent_Keys => Hash_Equivalent_Keys);

   package Response_Storage is new Ada.Containers.Hashed_Maps
     (Key_Type        => Action_ID_Type,
      Element_Type    => Stored_Response,
      Hash            => Hash_Function,
      Equivalent_Keys => Hash_Equivalent_Keys);

   Reponse_Tickets : Response_Ticket_Storage.Map :=
                       Response_Ticket_Storage.Empty_Map;
   --  TODO: Rip this out of the library

   Responses : Response_Storage.Map := Response_Storage.Empty_Map;

end AMI.Response;
