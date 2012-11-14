-------------------------------------------------------------------------------
--                                                                           --
--                                   AMI                                     --
--                                                                           --
--                                 Client                                    --
--                                                                           --
--                                  SPEC                                     --
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
package AMI.Client is

   CONNECT_TIMEOUT : exception;
   CONNECT_FAILED  : exception;
   
   type Autenticated_Type is (Unknown, No_Reply, 
  			      Authenticated, Not_Authenticated);
   type Client_Type is tagged
      record
	 Connected       : Boolean := False;
	 Server_Greeting : Ada.Strings.Unbounded.Unbounded_String;
	 Authenticated   : Autenticated_Type := Unknown;
	 Socket          : AWS.Net.Std.Socket_Type;
      end record; 
   
   
   procedure Connect (Client   : access Client_Type;
		      Hostname : in   String;
		      Port     : in     Natural); 

   procedure Disconnect (Client : access Client_Type); 

   procedure Wait_For_Disconnect (Client : access Client_Type);
   --  Blocking call that does no return until socket is in Error state

   procedure Send (Client : access Client_Type; 
		   Item   : in     String);
   -- Send an abitrary string

end AMI.Client;
