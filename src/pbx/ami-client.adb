-------------------------------------------------------------------------------
--                                                                           --
--                                   AMI                                     --
--                                                                           --
--                               AMI.Client                                  --
--                                                                           --
--                                  BODY                                     --
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

with AWS.Net;
with AWS.Net.Buffered;

with Common;
with System_Messages;

package body AMI.Client is
   use Ada.Strings.Unbounded;
   use System_Messages;
   use Common;

   procedure Connect (Client   : in out Client_Type;
                      Hostname : in     String;
                      Port     : in     Natural) is
      use type Time;
      Timeout   : constant Time := Current_Time + 3.0;
   begin
      AWS.Net.Buffered.Shutdown (Client.Socket);

      Client.Connected := False;
      Client.Authenticated := Unknown;

      System_Messages.Notify (Debug, "Connecting to " &
                                Hostname & ":" &
                                Positive'Image (Port));
      AWS.Net.Std.Connect (Socket => Client.Socket,
                           Host   => Hostname,
                           Port   => Port,
                           Wait   => False);

      Wait_For_Connection :
      loop
         Client.Connected := Client.Is_Connected;
         exit Wait_For_Connection when Client.Connected
           or Current_Time > Timeout;
         delay 0.05;
      end loop Wait_For_Connection;

      if Client.Connected then
         System_Messages.Notify (Information, "Connected to " &
                                   Hostname & ":" &
                                   Positive'Image (Port));

         --  The first line in the transmission is the greeting
         Client.Server_Greeting :=
           To_Unbounded_String (AWS.Net.Buffered.Get_Line
                                  (Socket => Client.Socket));
         System_Messages.Notify (Debug, "Connect: Server greeted me with:" &
                                   To_String (Client.Server_Greeting));
         --  Call the On_Connect handler
         Client.On_Connect_Handler.all;
      else
         System_Messages.Notify
           (Information, "Connection timed out connecting to " &
                                   Hostname & ":" &
                                   Positive'Image (Port));
         raise CONNECT_TIMEOUT;
      end if;
   exception
      when others =>
         --  Synchronize the state
         Client.Connected := False;
         Client.Authenticated := Unknown;
         Client.On_Disconnect_Handler.all;
         raise;
   end Connect;

   function Connected (Client : in out Client_Type) return Boolean is
   begin
      return Client.Connected;
   end Connected;

   function Create (On_Connect    : in Connection_Event_Handler;
                    On_Disconnect : in Connection_Event_Handler)
                   return Client_Type is
      Socket : AWS.Net.Std.Socket_Type;
   begin
      return (Connected             => False,
              Server_Greeting       => Null_Unbounded_String,
              Authenticated         => Unknown,
              Socket                => Socket,
              On_Connect_Handler    => On_Connect,
              On_Disconnect_Handler => On_Disconnect);
   end Create;

   procedure Disconnect (Client : in out Client_Type) is
   begin
      AWS.Net.Buffered.Shutdown (Client.Socket);
   end Disconnect;

   function Get_Line (Client : in out Client_Type) return String is
   begin
      return AWS.Net.Buffered.Get_Line
        (Socket => Client.Socket);
   exception
      when others =>
         Client.On_Disconnect_Handler.all;
         raise GET_LINE_FAILED;
   end Get_Line;

   function Is_Connected (Client  : in out Client_Type) return Boolean is
      Socket_Event : AWS.Net.Event_Set;
   begin
      Socket_Event := AWS.Net.Check
        (Socket  => Client.Socket,
         Events  => (AWS.Net.Input => True, AWS.Net.Output => True));

      if Socket_Event (AWS.Net.Input) and then
        Socket_Event (AWS.Net.Output) then
         return True;
      else
         return False;
      end if;
   end Is_Connected;
   --  Determines if a server is connected via a socket.

   procedure Send (Client : in out Client_Type;
                   Item   : in     String) is
   begin
      AWS.Net.Buffered.Put (Client.Socket, Item);
      AWS.Net.Buffered.Flush (Client.Socket);
      System_Messages.Notify (Debug, "Send: Sent " & Item);
   exception
      when others =>
         Client.On_Disconnect_Handler.all;
   end Send;

   procedure Set_Connection_State (Client    : in out Client_Type;
                                   New_State : in     Boolean) is
   begin
      Client.Connected := New_State;
   end Set_Connection_State;

   procedure Wait_For_Connection (Client  : access Client_Type;
                                  Timeout : in     Duration := 3.0) is
      use type Time;
      Absolute_Timeout   : constant Time := Current_Time + Timeout;
   begin
      loop
         exit  when Client.Connected or Current_Time > Absolute_Timeout;
         delay 0.05;
      end loop;

      if not Client.Connected then
         raise AMI.Client.TIMEOUT;
      end if;
   end Wait_For_Connection;
end AMI.Client;
