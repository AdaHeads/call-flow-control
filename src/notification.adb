with Black.HTTP,
     Black.MIME_Types,
     Black.Response;

with Configuration,
     HTTP.Client,
     System_Messages;

with Handlers.Event_Socket;

package body Notification is

   procedure Broadcast (Item : in GNATCOLL.JSON.JSON_Value) is
      --  Context : constant String := "Notification.Broadcast";
   begin
      Handlers.Event_Socket.Broadcast (Item => Item);
   end Broadcast;

   procedure HTTP_Broadcast (Item : in GNATCOLL.JSON.JSON_Value) is
      Context : constant String := "Notification.Broadcast";
      use type Black.HTTP.Statuses;
   begin
      declare
         Reply : constant Black.Response.Class :=
           HTTP.Client.Post
             (URL          => Configuration.Notification_Broadcast_URL &
                              "?token=" & Configuration.Server_Token,
              Data         => Item.Write,
              Content_Type => Black.MIME_Types.Application.JSON);
      begin
         if Reply.Status /= Black.HTTP.OK then
            System_Messages.Critical
              (Message => "Notification.Broadcast failed.  " &
                          "This JSON object was not transmitted: " &
                          Item.Write & "  Error message: " & Reply.Content,
               Context => Context);
         end if;
      end;
   exception
      when Event : others =>
         System_Messages.Critical_Exception
           (Message => "Unexpected exception.",
            Event   => Event,
            Context => Context);
         raise;
   end HTTP_Broadcast;
end Notification;
