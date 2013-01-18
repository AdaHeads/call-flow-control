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

with Ada.Strings.Unbounded;
with AWS.Net.Std;

with AMI.Packet.Action;
with AMI.Parser;

package AMI.Client is

   TIMEOUT         : exception;
   CONNECT_TIMEOUT : exception;
   CONNECT_FAILED  : exception;
   GET_LINE_FAILED : exception;
   SEND_FAILED     : exception;

   type Autenticated_Type is (Unknown, No_Reply,
                              Authenticated, Not_Authenticated);
   --   type Client_Type is limited private;
   --  TODO: Move to private.
   --  type Socket_Type is new AWS.Net.Std.Socket_Type with null record;

   type Connection_Event_Handler is not null access procedure;

   procedure Null_Callback is null;

   --  TODO: Make limited.
   type Client_Type is tagged
      record
         Connected             : Boolean := False;
         Server_Greeting       : Ada.Strings.Unbounded.Unbounded_String;
         Authenticated         : Boolean := False;
         Socket                : AWS.Net.Std.Socket_Type;
         On_Connect_Handler    : Connection_Event_Handler
           := Null_Callback'Access;
         On_Disconnect_Handler : Connection_Event_Handler
           := Null_Callback'Access;
      end record;

   function Create (On_Connect    : in Connection_Event_Handler;
                    On_Disconnect : in Connection_Event_Handler)
                   return Client_Type;

   procedure Connect (Client   : in out Client_Type;
                      Hostname : in   String;
                      Port     : in     Natural);

   procedure Disconnect (Client : in out Client_Type);

   function Get_Line (Client : in out Client_Type) return String;

   procedure Send (Client : in out Client_Type;
                   Item   : in     String);
   --  Send an abitrary string

   procedure Send (Client : in out Client_Type;
                   Item   : in     AMI.Packet.Action.Request);

   function Send (Client : in out Client_Type;
                  Item   : in     AMI.Packet.Action.Request)
                  return AMI.Parser.Packet_Type;
   --  Synchronous send

   procedure Send (Client : in out Client_Type;
                   Item   : in     AMI.Packet.AMI_Packet);

   function Is_Connected (Client  : in out  Client_Type) return Boolean;

   --  TODO: Deprecate this.
   function Connected (Client : in out Client_Type) return Boolean;
   --  TODO: Deprecate this.
   procedure Set_Connection_State (Client    : in out Client_Type;
                                   New_State : in     Boolean);

   procedure Wait_For_Connection (Client  : access Client_Type;
                                  Timeout : in     Duration := 3.0);
   --  Waits for a client to establish a connection for duration.
   --  Raises TIMEOUT if the connection is not established within the
   --  given duration.

private
   --  TODO: Move the client here.
end AMI.Client;
