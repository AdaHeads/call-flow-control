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

with Ada.Calendar;
with Ada.Text_IO;
with Ada.Exceptions;

with AWS.Net;
with AWS.Net.Buffered;

with System_Messages;

package body AMI.Client is
   use System_Messages;
   use Ada.Strings.Unbounded;

   function Is_Connected (Client  : access Client_Type) return Boolean;
   
   function Is_Connected (Client  : access Client_Type) return Boolean is
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

   procedure Connect (Client   : access Client_Type;
		      Hostname : in   String;
		      Port     : in     Natural) is
      use Ada.Calendar;
      function Current_Time return Ada.Calendar.Time renames Clock;
      
      Timeout   : constant Ada.Calendar.Time := Current_Time + 3.0;
   begin
      AWS.Net.Buffered.Shutdown (Client.Socket);
      
      Client.Connected := False;
      Client.Authenticated := Unknown;
      
      System_Messages.Notify (Debug, "Connecting to " & 
				Hostname & ":" &
				Positive'Image(Port));
      AWS.Net.Std.Connect (Socket => Client.Socket,
			   Host   => Hostname,
			   Port   => Port,
			   Wait   => False);
      Wait_For_Connection :
      loop
	 Client.Connected := Is_Connected(Client);
	 exit Wait_For_Connection when Client.Connected 
	   or Current_Time > Timeout;
	 delay 0.05;
      end loop Wait_For_Connection;
      
      if Client.Connected then
         System_Messages.Notify (Information, "Connected to " & 
                                   Hostname & ":" &
                                   Positive'Image(Port));

         --  The first line in the transmission is the greeting
         Client.Server_Greeting := 
           To_Unbounded_String (AWS.Net.Buffered.Get_Line 
                                  (Socket => Client.Socket));
         System_Messages.Notify (Debug, "Connect: Server greeted me with:" & 
                                   To_String (Client.Server_Greeting));
         
      else
         System_Messages.Notify (Information, "Connection timed out connecting to " & 
                                   Hostname & ":" &
                                   Positive'Image(Port));
         raise CONNECT_TIMEOUT;
      end if;
   exception
      when Error: others =>
	 Client.Connected := False;
	 Client.Authenticated := Unknown;
	 raise CONNECT_FAILED with Ada.Exceptions.Exception_Message(Error);
   end Connect;

   --  Does not have the desired effect :-(
   procedure Wait_For_Disconnect (Client : access Client_Type) is
      Socket_Event : AWS.Net.Event_Set;       
   begin
      Socket_Event := AWS.Net.Poll
	(Socket  => Client.Socket,
	 Events  => (AWS.Net.Error  => True,
                     AWS.Net.Input  => True,
                     AWS.Net.Output => True),
         Timeout => AWS.Net.Forever);
   end Wait_For_Disconnect;

   procedure Send (Client : access Client_Type; 
		   Item   : in     String) is
   begin
      AWS.Net.Buffered.Put (Client.Socket, Item);
      AWS.Net.Buffered.Flush (Client.Socket);
      System_Messages.Notify (Debug, "Send: Sent " & Item);
   exception
      when Error: others =>
	 Ada.Text_IO.Put ("Send: Unexpected exception: ");
	 Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information(Error));
	 raise AMI_SOCKET_NOT_CONNECTED;
   end Send;
   
   procedure Disconnect (Client : access Client_Type) is
   begin
      AWS.Net.Buffered.Shutdown (Client.Socket);
   end Disconnect;
end AMI.Client;   
