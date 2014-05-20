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

with Ada.Strings.Unbounded,
     Model.Call,
     Model.User,
     Model.Peer.List,
     PBX,
     PBX.Action,
     Request_Utilities,
     Response.Templates,
     System_Messages,
     View;

package body Handlers.Call.Pickup is
   use System_Messages,
       View,
       Model;

   function Callback return HTTP.Callback is
   begin
      return Generate_Response'Access;
   end Callback;

   -------------------------
   --  Generate_Response  --
   -------------------------

   function Generate_Response (Request : Black.Request.Instance)
                               return Black.Response.Class
   is
      use Ada.Strings.Unbounded;
      use type Model.Call.Instance;

      Context : constant String := Package_Name & ".Generate_Response";

      User              : Model.User.Instance
      renames Request_Utilities.User_Of (Request);
      Assigned_Call     : Model.Call.Instance;
      Call_ID_Param    : String renames Request.Parameter
        (Key => Call_ID_String,
         Default => Model.Call.Null_Identification.Image);
      Call_ID          : Model.Call.Identification :=
        Model.Call.Null_Identification;
   begin
      if not User.Peer.Registered then
         System_Messages.Error
           (Message => "User has no peer unavailable " & User.Image,
            Context => Context);

         return Response.Templates.Bad_Parameters
           (Request       => Request,
            Response_Body => Description ("User has no peer unavailable"));
      end if;

      if Request.Has_Parameter (Key => Call_ID_String) then
         Call_ID := Model.Call.Value (Call_ID_Param);
         System_Messages.Debug
           (Message => "Picking call " & Call_ID.Image,
            Context => Context);
      end if;

      User.Park_Current_Call; --  Park any calls that the user currently has.

      Model.Call.Assign_Call (To   => User.Identification,
                              ID   => Call_ID,
                              Call => Assigned_Call);

      System_Messages.Debug
        (Message => "Assign_Call returns call " &
           Assigned_Call.To_JSON.Write,
         Context => Context);

      if Assigned_Call = Model.Call.Null_Instance then
         return Response.Templates.Not_Found (Request);
      end if;

      System_Messages.Debug
        (Message => "Assigning call " &
           Assigned_Call.ID.Image &
           " to user" &
           User.Identification'Img,
         Context => Context);

      PBX.Action.Transfer (Assigned_Call.ID, User);

      pragma Assert (Assigned_Call.Assigned_To /=
                       Model.User.Null_Identification);

      return Response.Templates.OK
        (Request       => Request,
         Response_Body => Assigned_Call.To_JSON);

   exception

      when Model.Call.Not_Found =>
         return Response.Templates.Not_Found
           (Request       => Request,
            Response_Body => Description ("Call not found " & Call_ID_Param));

      when Model.Call.Not_Available =>
         return Response.Templates.Bad_Parameters
           (Request       => Request,
            Response_Body => Description ("Call is not available to user."));

      when Model.Peer.Peer_Not_Registered =>
         System_Messages.Error
           (Message         => "User has no peer registered.",
            Context => Context);
         return Response.Templates.Server_Error
           (Request       => Request,
            Response_Body => View.Description
              (Message => "User has no peer registered"));

      when Model.Peer.List.Not_Found =>
         System_Messages.Error
           (Message         => "No peer to user found in peer cache!",
            Context => Context);
         return Response.Templates.Server_Error
           (Request       => Request,
            Response_Body => View.Description
              (Message => "User has no peer registered"));

      when E : others =>
         System_Messages.Critical_Exception
           (Event           => E,
            Message         => "Pickup failed.",
            Context => Context);
         return Response.Templates.Server_Error (Request);

   end Generate_Response;

   ---------------
   --  Handler  --
   ---------------

   function Handler (Client_Request : in Request.Instance)
                     return Response.Instance is
      use Model.Call;

      Context : constant String := Package_Name & ".Handler";

      Assigned_Call    : Model.Call.Instance;
      User             : constant Model.User.Instance :=
        Client_Request.User;
   begin
      if not User.Peer.Registered then
            return Response.Create
              (Status      => Response.Bad_Request,
               Description => "User has no peer unavailable " & User.Image);
      end if;

      Client_Request.User.Park_Current_Call;
      --  Park any calls that the user currently has (Implicit park).

      declare
         Call_ID : constant Model.Call.Identification :=
           Value (Client_Request.Parameter ("call_id"));
      begin

         Model.Call.Assign_Call (To   => User.Identification,
                                 ID   => Call_ID,
                                 Call => Assigned_Call);

         System_Messages.Debug
           (Message => "Assign_Call returns call " &
              Assigned_Call.ID.Image,
            Context => Context);

         if Assigned_Call = Model.Call.Null_Instance then
            return Response.Create
              (Status    => Response.Not_Found,
               Description => "No calls available.");
         end if;

         System_Messages.Debug
           (Message => "Assigning call " & Assigned_Call.ID.Image &
                       " to user"        & User.Identification'Img,
            Context => Context);

         PBX.Action.Transfer (Assigned_Call.ID, User);

         pragma Assert (Assigned_Call.Assigned_To /=
                          Model.User.Null_Identification);

         return Response.Create
           (Status    => Response.Success,
            With_Body => Assigned_Call.To_JSON);
      end;
   exception
      when Model.Call.Not_Found =>
         return Response.Create
           (Status      => Response.Not_Found,
            Description => "Call not found");
      when Constraint_Error =>
         return Response.Create
           (Status      => Response.Bad_Request,
            Description => "Bad parameters.");

   end Handler;

end Handlers.Call.Pickup;
