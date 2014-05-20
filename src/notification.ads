with
  GNATCOLL.JSON;

package Notification is
   procedure Broadcast (Item : in     GNATCOLL.JSON.JSON_Value);

   procedure HTTP_Broadcast (Item : in GNATCOLL.JSON.JSON_Value);
end Notification;
