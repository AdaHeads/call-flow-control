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
     PBX,
     PBX.Action,
     System_Messages;

package body Handlers.Call.Pickup_Next is
   use System_Messages,
       Model;

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

      Model.Call.Assign_Call (To   => User.Identification,
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

end Handlers.Call.Pickup_Next;
