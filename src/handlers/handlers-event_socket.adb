with Ada.Containers.Indefinite_Hashed_Sets;
with Ada_2012.Strings.Hash_Case_Insensitive;
with Ada_2012.Strings.Equal_Case_Insensitive;
with System_Messages;

package body Handlers.Event_Socket is
   use System_Messages;

   function "=" (Left, Right : in Socket_Type) return Boolean;

   function Hash (Item : in Socket_Type) return Ada.Containers.Hash_Type;

   package Client_Storage is new Ada.Containers.Indefinite_Hashed_Sets
     (Element_Type        => Socket_Type,
      Hash                => Hash,
      Equivalent_Elements => "=",
      "="                 => "=");

   Clients : Client_Storage.Set := Client_Storage.Empty_Set;

   function "=" (Left, Right : in Socket_Type) return Boolean is
   begin
      return Ada_2012.Strings.Equal_Case_Insensitive
        (Left  => Image (Left),
         Right => Image (Right));
   end "=";

   procedure Broadcast (Item : in GNATCOLL.JSON.JSON_Value) is
      use Client_Storage;

      Context : constant String := Package_Name & ".Broadcast";

      C : Cursor := Clients.First;
   begin
      Debug ("Sending events", Context);
      while C /= No_Element loop
         begin
            Debug ("Sending event to " & Image (Element (C)), Context);
            String'Write (Stream (Element (C)),
                          Item.Write & ASCII.LF);
            C := Next (C);
         exception
            when others =>
               Unregister_Client (Element (C));
               C := Next (C);
         end;
      end loop;
   end Broadcast;

   function Hash (Item : in Socket_Type) return Ada.Containers.Hash_Type is
   begin
      return Ada_2012.Strings.Hash_Case_Insensitive (Image (Item));
   end Hash;

   procedure Register_Client (Client : in Socket_Type) is
      Context : constant String := Package_Name & ".Register_Client";
   begin
      Debug (Image (Client), Context);

      Clients.Insert (Client);
   end Register_Client;

   procedure Unregister_Client (Client : in Socket_Type) is
      Context : constant String := Package_Name & ".Unregister_Client";
   begin
      Debug (Image (Client), Context);
      Clients.Delete (Client);
   end Unregister_Client;

end Handlers.Event_Socket;
