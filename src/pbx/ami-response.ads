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

   Timeout : exception;

   procedure Subscribe (Reply_For : in Request);
   --  Subscribe for a reply on the given request.

   procedure Wait_For (Action_ID : in Action_ID_Type;
                      Timeout   : in Duration := 3.0);
   --  Provides an explicit synchonization mechanism

   procedure Notify (Packet : in AMI.Parser.Packet_Type);
   --  Notify subscribers about a reposense
private
   function Hash_Function (Key : in Action_ID_Type)
                           return Ada.Containers.Hash_Type;
   function Hash_Equivalent_Keys
     (Left, Right : in Action_ID_Type)
      return Boolean;

   package Response_List_Type is new Ada.Containers.Hashed_Maps
     (Key_Type => Action_ID_Type,
      Element_Type => AMI.Packet.Action.Response_Handler_Type,
      Hash => Hash_Function,
      Equivalent_Keys => Hash_Equivalent_Keys);

   Reponse_List : Response_List_Type.Map;
   --  TODO: Rip this out of the library

end AMI.Response;
