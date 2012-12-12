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
               Client.Authenticated := True;

            elsif To_String (Value) = Authentication_Failed_String then
               Client.Authenticated := False;
               System_Messages.Notify (Error, "Login_Callback: " &
                                         Authentication_Failed_String);
            else
               Client.Authenticated := True;
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
