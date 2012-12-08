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

with AMI.Packet.Field;

package AMI.Packet.Action is
   use AMI.Packet.Field;

   type Valid_Action is (Undefined, Login, Logoff, Ping);

   type Response_Handler_Type is not null access procedure;

   procedure Null_Reponse_Handler is null;

   type Request (Asynchronous : Boolean)
     is tagged limited private;

   function To_AMI_Packet (R : in Request) return AMI_Packet;

   procedure Add_Field (R : in out Request;
                        F : in     AMI.Packet.Field.Field);
   --  Add a custom field.

   function Ping (On_Response : in Response_Handler_Type :=
                    Null_Reponse_Handler'Access) return Request;

   Null_Request : constant Request;
private

   type Request (Asynchronous : Boolean) is tagged limited
      record
         Action           : Valid_Action;
         Action_ID        : Action_ID_Type := Next;
         Fields           : Field_List.List;
         Response_Handler : Response_Handler_Type;
   end record;

   Null_Request : constant Request :=
                    (Asynchronous     => True,
                     Action           => Undefined,
                     Action_ID        => Null_Action_ID,
                     Fields           => Field_List.Empty_List,
                     Response_Handler => Null_Reponse_Handler'Access);

end AMI.Packet.Action;
