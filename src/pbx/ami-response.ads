-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                               AMI.Reponse                                 --
--                                                                           --
--                                  SPEC                                     --
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

with AMI.Callback;
with AMI.Parser;
with AMI.Client;
package AMI.Response is 
   use AMI.Callback;
   use AMI.Client;
   procedure Subscribe (Action_ID : in Action_ID_Type;
		        Callback  : in Callback_Type);
   --  Subscribe for a reply with the given action ID.
   
   function Wait_For (Action_ID : in Action_ID_Type;
		      Timeout   : in Duration := 3.0) return Boolean;
   --  Provides an explicit synchonization mechanism

   procedure Notify (Client : access Client_Type;
		     Packet : in     AMI.Parser.Packet_Type);
   --  Notify about a reposense
   
   procedure Wait_For (Action_ID : in Action_ID_Type);
   --  Block a thread untill an reponse with action ID occurs
   
private
   function Hash_Function (Key : in Action_ID_Type)
			  return Ada.Containers.Hash_Type;
   function Hash_Equivalent_Keys 
     (Left, Right : in Action_ID_Type)
     return Boolean;
   
   package Response_List_Type is new Ada.Containers.Hashed_Maps
     (Key_Type => Action_ID_Type,
      Element_Type => Callback_Type,
      Hash => Hash_Function,
      Equivalent_Keys => Hash_Equivalent_Keys);
   
   Reponse_List : Response_List_Type.Map;
   --  TODO: Rip this out of the library
   
   --  protected Waiter is
   --     entry Wait_For (Key : in Action_ID_Type);
   --     procedure Signal (Key : in Action_ID_Type);
   --  private
   --     Connected : Boolean := False;
   --  end Connection;
   
   
end AMI.Response;
