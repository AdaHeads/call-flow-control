-------------------------------------------------------------------------------
--                                                                           --
--                     Copyright (C) 2014-, AdaHeads K/S                     --
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

with Model.Call,
     Model.User,
     Model.Peer,
     Model.User.List,
     PBX,
     PBX.Action,
     Request_Utilities,
     Response.Templates,
     System_Messages,
     View;

package body Handlers.Call.Originate is
   use AWS.Status,
       System_Messages,
       View,
       Model;

   function Callback return AWS.Response.Callback is
   begin
      return Generate_Response'Access;
   end Callback;

   -------------------------
   --  Generate_Response  --
   -------------------------

   function Generate_Response
     (Request : in AWS.Status.Data) return AWS.Response.Data is
      use Model.Call,
          Model.User;

      Context           : constant String := Package_Name & ".Originate";

      Extension_String  : constant String :=
        Parameters (Request).Get ("extension");

      User              : constant Model.User.Instance :=
        Request_Utilities.User_Of (Request);
   begin

      if not User.Peer.Registered then
         raise Model.Peer.Peer_Not_Registered;
      end if;

      PBX.Action.Originate
        (User        => User,
         Extension   => Extension_String);

      return Response.Templates.OK (Request);
   exception
      when PBX.Action.Error =>
         System_Messages.Critical
           (Message         => "PBX failed to handle request.",
            Context => Context);
         return Response.Templates.Server_Error (Request);

      when Model.Peer.Peer_Not_Registered =>
         System_Messages.Error
           (Message         => "User has no peer registered.",
            Context => Context);
         return Response.Templates.Server_Error
           (Request       => Request,
            Response_Body => View.Description
              (Message => "User has no peer registered"));

      when Event : others =>
         System_Messages.Critical_Exception
           (Event           => Event,
            Message         => "Unhandled exception.",
            Context => Context);
         return Response.Templates.Server_Error (Request);

   end Generate_Response;
end Handlers.Call.Originate;
