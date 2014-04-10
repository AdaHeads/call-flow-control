with
  System_Messages;

package body Notification is
   procedure Broadcast (Item : in     GNATCOLL.JSON.JSON_Value) is
   begin
      System_Messages.Fixme
        (Message => "Not implemented.  " &
                    "This JSON object was not transmitted: " &
                    Item.Write,
         Context => "Notification.Broadcast");
   end Broadcast;
end Notification;
