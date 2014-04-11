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

with Black.HTTP;

with Handlers.Authenticated_Dispatcher,
     Model.User,
     System_Messages;

with Handlers.Call.Hangup,
     Handlers.Call.List,
     Handlers.Call.Originate,
     Handlers.Call.Park,
     Handlers.Call.Pickup,
     Handlers.Call.Queue,
     Handlers.Call.Transfer,
     Handlers.CORS_Preflight,
     Handlers.Debug,
     Handlers.Not_Found,
     Handlers.Users.List;
     --  Handlers.Users.Profile;

package body Handlers.Route is

   Handlers_Registered : Boolean := False;

   function Callback (Request : in Black.Request.Instance)
                     return Black.Response.Class is
   begin
      if Handlers_Registered then
         return Handlers.Authenticated_Dispatcher.Run (Request);
      else
         raise Program_Error
           with "Handlers not registered yet.";
      end if;
   end Callback;

   -----------------------------
   --  Permission_Operations  --
   -----------------------------

   package Permission_Operations is
      use Handlers.Authenticated_Dispatcher, Model;

      Public        : constant ACL := (Public => True);
      Receptionist  : constant ACL :=
        (Public => False,
         As     => (Model.User.Receptionist  => True,
                    others              => False));
      Service_Agent : constant ACL :=
        (Public => False,
         As     => (Model.User.Service_Agent => True,
                    others              => False));
      pragma Unreferenced (Service_Agent);

      Administrator : constant ACL :=
        (Public => False,
         As     => (Model.User.Administrator => True,
                    others             => False));

      function "or" (Left, Right : in ACL) return ACL;
      pragma Unreferenced ("or");
      function "or" (Left  : in Boolean;
                     Right : in ACL) return ACL;
      pragma Unreferenced ("or");
   end Permission_Operations;

   -----------------------------
   --  Permission_Operations  --
   -----------------------------

   package body Permission_Operations is
      function "or"  (Left, Right : in ACL) return ACL is
         use Model.User;
      begin
         if Left.Public or Right.Public then
            return Public;
         else
            return (Public => False,
                    As     => Left.As or Right.As);
         end if;
      end "or";

      function "or" (Left  : in Boolean;
                     Right : in ACL) return ACL is
      begin
         if Left then
            return Public;
         else
            return Right;
         end if;
      end "or";
   end Permission_Operations;

   procedure Register_Handlers is
      use Black.HTTP, Black.Request;
      use Handlers.Authenticated_Dispatcher;
      use Permission_Operations;

      Context : constant String := Package_Name & "Register_Handlers";
   begin
      if Handlers_Registered then
         return;
      end if;

      Handlers_Registered := True;
      Set_Default (Action => Not_Found.Callback);
      --  If no request methods has been set in the request we treat the
      --  request as failed and return a 404 Not Found.

      Set_Default (Method => Options,
                   Action => CORS_Preflight.Callback);
      --  This is for CORS preflight requests.

      pragma Style_Checks ("M100"); --  Allow long lines in the routing table

      Register (Get,  "/call/list",             Receptionist,  Call.List.Callback);
      Register (Get,  "/call/queue",            Receptionist,  Call.Queue.Callback);
      Register (Post, "/call/hangup",           Receptionist,  Call.Hangup.Callback);
      Register (Post, "/call/originate",        Receptionist,  Call.Originate.Callback);
      Register (Post, "/call/park",             Receptionist,  Call.Park.Callback);
      Register (Post, "/call/pickup",           Receptionist,  Call.Pickup.Callback);
      Register (Post, "/call/transfer",         Receptionist,  Call.Transfer.Callback);

      Register (Get,  "/users/list",            Administrator, Users.List.Callback);
      --  Register (Get,  "/users",                 Receptionist,  Users.Profile.Callback);

      Register (Get, "/debug/channel/list",     Public,        Debug.Channel_List'Access);
      Register (Get, "/debug/contact",          Public,        Debug.Contact'Access);
      Register (Get, "/debug/peer/list",        Public,        Debug.Peer_List'Access);
      Register (Get, "/debug/token/dummy_list", Public,        Debug.Dummy_Tokens'Access);

      pragma Style_Checks ("M79");

      System_Messages.Information (Message => "Registered request handlers.",
                                   Context => Context);
   end Register_Handlers;

end Handlers.Route;
