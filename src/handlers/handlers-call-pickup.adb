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
     Model.User.List,
     Model.Peer.List,
     PBX,
     PBX.Action,
     Request_Utilities,
     Response.Templates,
     System_Messages,
     View;

package body Handlers.Call.Pickup is
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

   function Generate_Response (Request : in AWS.Status.Data)
                               return AWS.Response.Data
   is
      use Ada.Strings.Unbounded;

      Context : constant String := Package_Name & ".Generate_Response";

      User              : Model.User.Instance
      renames Request_Utilities.User_Of (Request);
      Assigned_Call     : Model.Call.Instance;
   begin

      if Model.Call.List_Empty then
         return Response.Templates.Not_Found (Request);
      end if;

      if not User.Peer.Registered then
         System_Messages.Error
           (Message => "User has no peer unavailable " & User.Image,
            Context => Context);

         return Response.Templates.Not_Found
           (Request       => Request,
            Response_Body => Description ("User has no peer unavailable"));
      else

         if Parameters (Request).Exist (Call_ID_String) then
            declare
               Call_ID_Param    : String renames
                 Parameters (Request).Get (Name => Call_ID_String);
               Call_ID          : constant Model.Call.Identification :=
                 Model.Call.Value (Call_ID_Param);
            begin
               Assigned_Call := Model.Call.Get (Call => Call_ID);
            exception
               when Model.Call.Not_Found =>
                  return Response.Templates.Not_Found
                    (Request       => Request,
                     Response_Body => Description
                       ("call_id " & Call_ID_Param & " not found."));

            end;
         else
            Assigned_Call := Model.Call.Highest_Prioirity;
         end if;

         System_Messages.Debug
           (Message => "Assigning call " &
              Assigned_Call.To_JSON.Write &
              " to user " &
              User.To_JSON.Write,
            Context => Context);

         Model.User.List.Get_Singleton.Assign_Call
           (User_ID => User.Identification,
            Call_ID => Assigned_Call.ID);

         PBX.Action.Transfer (Assigned_Call.ID, User);

         return Response.Templates.OK
           (Request       => Request,
            Response_Body => Assigned_Call.To_JSON);
      end if;

   exception

      when Model.Call.Already_Bridged =>
         return Response.Templates.Bad_Parameters
           (Request       => Request,
            Response_Body => Description
              ("Agent tried to claim call that is already assigned"));

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
end Handlers.Call.Pickup;
