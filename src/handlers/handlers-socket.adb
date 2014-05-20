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

with Ada.Exceptions;

with Handlers.Call.Hangup,
     Handlers.Call.List,
     Handlers.Call.Originate,
     Handlers.Call.Park,
     Handlers.Call.Pickup,
     Handlers.Call.Pickup_Next,
     Handlers.Call.Queue,
     Handlers.Call.Transfer,
     Handlers.Peer,
     Handlers.Event_Socket,
     System_Messages;

package body Handlers.Socket is

   function Handle_Request
     (Client_Request : in Request.Instance)
      return Response.Instance is

      Server_Response : Response.Instance;

   begin
      case Client_Request.Resource is
         when Request.Call_List =>
            Server_Response :=  Call.List.Handler;

         when Request.Call_Queue  =>
            Server_Response :=  Call.Queue.Handler;

         when Request.Call_Hangup =>
            Server_Response := Call.Hangup.Handler
              (Client_Request => Client_Request);

         when Request.Event_Socket =>
            Event_Socket.Register_Client (Client_Request.Client);
            Server_Response :=  Response.Create (Status => Response.Success);
            Server_Response.Keep_Open := True;

         when Request.Call_Originate =>
            Server_Response :=  Call.Originate.Handler
              (Client_Request => Client_Request);

         when Request.Call_Park  =>
            Server_Response :=  Call.Park.Handler
              (Client_Request => Client_Request);

         when Request.Call_Pickup  =>
            Server_Response :=  Call.Pickup.Handler
              (Client_Request => Client_Request);

         when Request.Call_Pickup_Next  =>
            Server_Response :=  Call.Pickup_Next.Handler
              (Client_Request => Client_Request);

         when Request.Call_Transfer  =>
            Server_Response :=  Call.Transfer.Handler
              (Client_Request => Client_Request);

         when Request.Peer_List  =>
            Server_Response :=  Peer.List_Handler;

         when Request.Unknown  =>
            Server_Response :=  Response.Create (Status => Response.Not_Found);

            --           when others =>
--              raise Program_Error
--                with "Not implemented yet.";
      end case;

      return Server_Response;
   exception
      when Event : others =>
         Server_Response :=  Response.Create
           (Status     => Response.Internal_Error,
            Description => Ada.Exceptions.Exception_Information (Event));
         System_Messages.Debug
           (Message => Ada.Exceptions.Exception_Information (Event),
            Context => "Handle_Request");
      return Server_Response;

   end Handle_Request;
end Handlers.Socket;
