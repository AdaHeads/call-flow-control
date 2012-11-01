with System_Messages;
with Ada.Strings.Unbounded;

package body AMI.Callback is
   use System_Messages;

   Authentication_Accepted_String : constant String :=
     "Authentication accepted";
   Authentication_Failed_String   : constant String :=
     "Authentication failed";

   procedure Login_Callback (Client : access Client_Type;
                             Packet : in     Packet_Type) is
      use Ada.Strings.Unbounded;
      Value : Unbounded_String;
   begin
      if Client /= null and then Packet /= New_Packet then
         if Try_Get (Packet.Fields, Message, Value) then
            if To_String (Value) = Authentication_Accepted_String then
               Client.Authenticated := Authenticated;

            elsif To_String (Value) = Authentication_Failed_String then
               Client.Authenticated := Not_Authenticated;
               System_Messages.Notify (Error, "Login_Callback: " &
                                         Authentication_Failed_String);
            else
               Client.Authenticated := Unknown;
               System_Messages.Notify (Error, "Login_Callback: Bad Reply:" &
                                         To_String (Value));
            end if;
         end if;
      end if;
   end Login_Callback;

   procedure Null_Callback (Client : access Client_Type;
                            Packet : in     Packet_Type) is
   begin
      null;
   end Null_Callback;

   procedure Ping_Callback (Client : access Client_Type;
                            Packet : in     Packet_Type) is
   begin
      if Client /= null and then Packet /= New_Packet then
         System_Messages.Notify
           (Debug, "AMI.Callback.Ping_Callback: Got pong.");
      end if;
   end Ping_Callback;
end AMI.Callback;
