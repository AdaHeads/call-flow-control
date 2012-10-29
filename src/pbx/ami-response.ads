with Ada.Containers.Hashed_Maps;

with AMI.Callback;
with AMI.Parser;

package AMI.Response is 
   use AMI.Callback;
   
   procedure Subscribe (Action_ID : in Action_ID_Type;
		        Callback  : in Callback_Type);
   --  Subscribe for a reply with the given action ID.
   
   
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
   
   
   --  protected Waiter is
   --     entry Wait_For (Key : in Action_ID_Type);
   --     procedure Signal (Key : in Action_ID_Type);
   --  private
   --     Connected : Boolean := False;
   --  end Connection;
   
   
end AMI.Response;
