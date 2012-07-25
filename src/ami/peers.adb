with Ada.Text_IO;
with Ada.Calendar.Formatting; use  Ada.Calendar.Formatting;
package body Peers is

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
      else
         Ada.Text_IO.Put_Line
           ("Could not find an Extension for: " & Peer_String);
         Exten := Null_Unbounded_String;
      end if;
      return Exten;
   end Get_Exten;

   function Hash (Peer_Address : in Unbounded_String) return Hash_Type is
   begin
      return Ada.Strings.Hash (To_String (Peer_Address));
   end Hash;

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

end Peers;
