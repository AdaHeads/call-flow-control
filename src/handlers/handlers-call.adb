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
with Model.Call,
     Model.User,
     Model.User.List,
     PBX,
     PBX.Action,
     Request_Utilities,
     Response.Templates,
     System_Messages,
     View;

package body Handlers.Call is
   use AWS.Status,
       System_Messages,
       View,
       Model;

   --------------
   --  Hangup  --
   --------------

   function Hangup
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use Model.User;

      Context           : constant String := Package_Name & ".Hangup";
      Requested_Call_ID : String renames Parameters (Request).Get ("call_id");
   begin
      --  An Administrator is able to end any call.
      if Request_Utilities.User_Of (Request).Permissions (Administrator) then
         PBX.Action.Hangup (ID => Model.Call.Value
                            (Item => Requested_Call_ID));
      else
         --  TODO: Add a way of verifying that the calling user owns the call.
         PBX.Action.Hangup (ID => Model.Call.Value
                            (Item => Requested_Call_ID));

      end if;

      return Response.Templates.OK (Request);

   exception
      when Model.Call.Not_Found =>
         return Response.Templates.Not_Found (Request);

      when E : others =>
         System_Messages.Critical_Exception
           (Event           => E,
            Message         => "Hangup request failed for call id: " &
              Requested_Call_ID,
            Context => Context);
         return Response.Templates.Server_Error (Request);
   end Hangup;

   ------------
   --  List  --
   ------------

   function List (Request : in AWS.Status.Data) return AWS.Response.Data is
      Context : constant String := Package_Name & ".List";
   begin
      return Response.Templates.OK (Request       => Request,
                                    Response_Body => Model.Call.List);
   exception
      when E : others =>
         System_Messages.Critical_Exception
           (Event           => E,
            Message         => "Listing calls failed.",
            Context => Context);
         return Response.Templates.Server_Error (Request);
   end List;

   -----------------
   --  Originate  --
   -----------------

   function Originate (Request : in AWS.Status.Data) return AWS.Response.Data
   is
      use Model.Call,
          Model.User;

      Context           : constant String := Package_Name & ".Originate";

      Extension_String  : constant String :=
        Parameters (Request).Get ("extension");
   begin

      PBX.Action.Originate
        (User        => Request_Utilities.User_Of (Request),
         Extension   => Extension_String);

      return Response.Templates.OK (Request);
   exception
      when E : PBX.Action.Error =>
         System_Messages.Critical_Exception
           (Event           => E,
            Message         => "PBX failed to handle request.",
            Context => Context);
         return Response.Templates.Server_Error (Request);
      when E : others =>
         System_Messages.Critical_Exception
           (Event           => E,
            Message         => "Listing calls failed.",
            Context => Context);
         return Response.Templates.Server_Error (Request);

   end Originate;

   ------------
   --  Park  --
   ------------

   function Park (Request : in AWS.Status.Data)
                  return AWS.Response.Data is

      Context           : constant String := Package_Name & ".Park";

      Call_ID           : Model.Call.Identification renames
        Model.Call.Value (Parameters (Request).Get ("call_id"));
   begin

      --  Fetch the call from the call list.
      if not Model.Call.Has (ID => Call_ID) then
         return Response.Templates.Not_Found (Request);
      else

         PBX.Action.Park (Target  => Call_ID,
                          At_User => Request_Utilities.User_Of (Request));

         --  And let the user know that everything went according to plan.
         return Response.Templates.OK (Request);
      end if;
   exception
      when Model.Call.Invalid_ID =>
         return Response.Templates.Bad_Parameters (Request);
      when E : others =>
         System_Messages.Critical_Exception
           (Event           => E,
            Message         => "Park action failed.",
            Context => Context);
         return Response.Templates.Server_Error (Request);
   end Park;

   --------------
   --  Pickup  --
   --------------

   function Pickup
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use Ada.Strings.Unbounded;

      Context : constant String := Package_Name & ".Pickup";

      Call_ID_String    : String renames
        Parameters (Request).Get (Name => "call_id");
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

         if Call_ID_String /= "" then
            Assigned_Call := Model.Call.Get
              (Call => Model.Call.Value (Call_ID_String));
         else
            Assigned_Call := Model.Call.Highest_Prioirity;
         end if;

         Model.User.List.Get_Singleton.Assign_Call
           (User_ID => Request_Utilities.User_Of (Request).Identification,
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

      when E : others =>
         System_Messages.Critical_Exception
           (Event           => E,
            Message         => "Pickup failed.",
            Context => Context);
         return Response.Templates.Server_Error (Request);

   end Pickup;

   -------------
   --  Queue  --
   -------------
   --  TODO, extend the call list so that it will be able to return only
   --  inbound calls on this interface.

   function Queue
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      Context           : constant String := Package_Name & ".Queue";

   begin
      return Response.Templates.OK
        (Request       => Request,
         Response_Body => Model.Call.Queued_Calls);
   exception
      when E : others =>
         System_Messages.Critical_Exception
           (Event           => E,
            Message         => "Listing queued calls failed.",
            Context => Context);
         return Response.Templates.Server_Error (Request);

   end Queue;

   ----------------
   --  Transfer  --
   ----------------

   function Transfer
     (Request : in AWS.Status.Data)
         return AWS.Response.Data
   is
      use Model.Call;

      Context           : constant String := Package_Name & ".Transfer";

      Source          : Model.Call.Identification := Null_Identification;
      Destination     : Model.Call.Identification := Null_Identification;
   begin
      --  Check valitity of the call. (Will raise exception on invalid).
      Source      := Value (Parameters (Request).Get ("source"));
      Destination := Value (Parameters (Request).Get ("destination"));
      --        Destination := Agent.Get
      --          (PBX.Call.Get
      --             (Source).Assigned_To).Current_Call;
      if
        Source      = Null_Identification or
        Destination = Null_Identification
      then
         raise Model.Call.Invalid_ID;
      end if;

      PBX.Action.Bridge (Source      => Source,
                         Destination => Destination);

      return Response.Templates.OK (Request);
   exception
      when Invalid_ID =>
         return Response.Templates.Bad_Parameters
           (Request       => Request,
            Response_Body => Description ("Invalid or no call ID supplied"));

      when Model.Call.Not_Found =>
         return Response.Templates.Not_Found
           (Request       => Request,
            Response_Body => Description ("No call found with ID " &
                Parameters (Request).Get ("source")));

      when E : others =>
         System_Messages.Critical_Exception
           (Event           => E,
            Message         => "Transfer request failed.",
            Context => Context);
         return Response.Templates.Server_Error (Request);
   end Transfer;

end Handlers.Call;
