with Ada.Calendar.Formatting,
     Ada.Text_IO;

with Yolk.Log;
package body Peers is
   use  Ada.Calendar.Formatting;
   ----------------------------------------------------------------------------
   --  TODO Navngivning, Der er brug for nogle bedre navne her.
   protected Peers_List is
      function Get_Peers_List return Peer_List_Type.Map;
      procedure Replace_Peer (Item     : in Peer_Type);
      procedure Insert (New_Item : in Peer_Type);
   private
      List : Peer_List_Type.Map;
   end Peers_List;

   protected body Peers_List is
      function Get_Peers_List return Peer_List_Type.Map is
      begin
         return List;
      end Get_Peers_List;

      procedure Insert (New_Item : in Peer_Type) is
      begin
         Yolk.Log.Trace (Yolk.Log.Debug, "Inserted a new peer: " &
                           To_String (New_Item.Peer));
         Peer_List_Type.Insert (Container => List,
                                Key       => New_Item.Peer,
                                New_Item  => New_Item);
      end Insert;

      procedure Replace_Peer (Item : in Peer_Type) is
         use Peer_List_Type;
         Peer_Cursor : constant Peer_List_Type.Cursor :=
           Peer_List_Type.Find (List, Item.Peer);
      begin
         if Peer_Cursor /= Peer_List_Type.No_Element then
            Peer_List_Type.Replace_Element (Container => List,
                                            Position  => Peer_Cursor,
                                            New_Item  => Item);
         end if;

      end Replace_Peer;
   end Peers_List;

   --  TODO change it to use a database.
   function Get_Exten (Peer : in Unbounded_String) return Unbounded_String is
      Exten : Unbounded_String;
      Peer_String : constant String := To_String (Peer);
   begin
      if Peer_String = "SIP/softphone1" then
         Exten := To_Unbounded_String ("100");
      elsif Peer_String = "SIP/softphone2" then
         Exten := To_Unbounded_String ("101");
      elsif Peer_String = "SIP/DesireZ" then
         Exten := To_Unbounded_String ("102");
      elsif Peer_String = "SIP/TP-Softphone" then
         Exten := To_Unbounded_String ("103");
      elsif Peer_String = "SIP/JSA-N900" then
         Exten := To_Unbounded_String ("104");
      else
         Ada.Text_IO.Put_Line
           ("Could not find an Extension for: " & Peer_String);
         Exten := Null_Unbounded_String;
      end if;
      return Exten;
   end Get_Exten;

   function Get_Peers_List return Peer_List_Type.Map is
   begin
      return Peers_List.Get_Peers_List;
   end Get_Peers_List;

   function Get_PhoneInfo (Peer : in Peer_Type) return Unbounded_String is
   begin
      return Peer.ChannelType & To_Unbounded_String ("/") & Peer.Peer;
   end Get_PhoneInfo;

   function Hash (Peer_Address : in Unbounded_String) return Hash_Type is
   begin
      return Ada.Strings.Hash (To_String (Peer_Address));
   end Hash;

   procedure Insert_Peer (New_Item : in Peer_Type) is
   begin
      Peers_List.Insert (New_Item);
   end Insert_Peer;

   procedure Print_Peer (Peer : in Peer_Type) is
      use Ada.Text_IO;
   begin
      Put ("Peer => "   & To_String (Peer.Peer) & ", ");
      case Peer.Status is
      when Unregistered =>
         Put ("Status => Unregistered, ");
      when Registered =>
         Put ("Status => Registered, ");
         --      when others =>
         --         raise PROGRAM_ERROR;
      end case;

      Put ("Address => " & To_String (Peer.Address) & ", ");
      Put ("Channel_Type => " & To_String (Peer.ChannelType) & ", ");
      Put ("Port => " & To_String (Peer.Port) & ", ");
      Put ("Exten => " & To_String (Peer.Exten) & ", ");
      Put ("Last_Seen => " & Ada.Calendar.Formatting.Image (Peer.Last_Seen));
      New_Line;

   end Print_Peer;

   procedure Replace_Peer (Item : in Peer_Type) is
   begin
      Peers_List.Replace_Peer (Item);
   end Replace_Peer;

   procedure Set_PhoneInfo (Peer : in out Peer_Type;
                            Text : in Unbounded_String)
                               is
      Seperator_Index : Integer;
   begin

      if Ada.Strings.Unbounded.Count (Text, "/") > 0 then
         Seperator_Index := Index (Text, "/");
         Peer.Peer := Tail (Source => Text,
                      Count  => Length (Text) - Seperator_Index);
         Peer.ChannelType := Head (Text, Seperator_Index - 1);
         if To_String (Peer.ChannelType) /= "SIP" then
            Yolk.Log.Trace (Yolk.Log.Alert, To_String (Peer.ChannelType));
         end if;
      else
         Yolk.Log.Trace (Yolk.Log.Debug,
                       "Set_PhoneInfo: This peer does not have a Channeltype: "
                         & To_String (Text));
      end if;
   end Set_PhoneInfo;
end Peers;
