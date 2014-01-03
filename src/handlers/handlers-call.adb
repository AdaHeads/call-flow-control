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

with Ada.Strings.Unbounded,
     Ada.Exceptions;

with Common,
     HTTP_Codes,
     Model.User,
     Model.User.List,
     PBX,
     PBX.Action,
     PBX.Call,
     Request_Utilities,
     Response,
     System_Messages,
     System_Message.Critical,
     View.Call;

package body Handlers.Call is
   use AWS.Status,
       Common,
       System_Messages,
       View.Call,
       Model;

   package HTTP renames HTTP_Codes;

   --------------
   --  Hangup  --
   --------------

   function Hangup
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use Model.User;

      Response_Object   : Response.Object := Response.Factory (Request);
      Requested_Call_ID : String renames Parameters (Request).Get ("call_id");
   begin
      --  An Administrator is able to end any call.
      if Request_Utilities.User_Of (Request).Permissions (Administrator) then
         PBX.Action.Hangup (ID => PBX.Call.Value (Item => Requested_Call_ID));
      else
         --  TODO: Add a way of verifying that the calling user owns the call.
         PBX.Action.Hangup (ID => PBX.Call.Value (Item => Requested_Call_ID));

      end if;

      Response_Object.HTTP_Status_Code (HTTP.OK);
      Response_Object.Content (Status_Message
                               ("Status", "Hangup sent!"));

      return Response_Object.Build;

   exception
      when PBX.Call.Not_Found =>
         Response_Object.HTTP_Status_Code (HTTP.Not_Found);
         Response_Object.Content
           (Status_Message
              ("status", "not found"));
         return Response_Object.Build;

      when E : others =>
         System_Message.Critical.Response_Exception
           (Event           => E,
            Message         => "Hangup failed",
            Response_Object => Response_Object);
         return Response_Object.Build;
   end Hangup;

   ------------
   --  List  --
   ------------

   function List
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      Response_Object : Response.Object := Response.Factory (Request);
   begin
      Response_Object.HTTP_Status_Code (HTTP.OK);
      Response_Object.Content (PBX.Call.List);

      return Response_Object.Build;
   exception
      when E : others =>
         System_Message.Critical.Response_Exception
           (Event           => E,
            Message         => "List failed",
            Response_Object => Response_Object);
         return Response_Object.Build;
   end List;

   -----------------
   --  Originate  --
   -----------------

   function Originate
     (Request : in AWS.Status.Data)
      return AWS.Response.Data is
      use PBX.Call;
      use Model.User;

      Extension_String  : constant String :=
        Parameters (Request).Get ("extension");
      Response_Object   : Response.Object := Response.Factory (Request);
   begin

      PBX.Action.Originate
        (User        => Request_Utilities.User_Of (Request),
         Extension   => Extension_String);

      Response_Object.HTTP_Status_Code (HTTP.OK);
      Response_Object.Content (Status_Message
                               ("status", "ok"));

      return Response_Object.Build;
   exception
      when E : PBX.Action.Error =>
         System_Message.Critical.Response_Exception
           (Event           => E,
            Message         => Ada.Exceptions.Exception_Message (E),
            Response_Object => Response_Object);
         return Response_Object.Build;
      when E : others =>
         System_Message.Critical.Response_Exception
           (Event           => E,
            Message         => "Originate failed",
            Response_Object => Response_Object);
         return Response_Object.Build;

   end Originate;

   ------------
   --  Park  --
   ------------

   function Park (Request : in AWS.Status.Data)
                  return AWS.Response.Data is
      package Call_List renames PBX.Call;

      Response_Object   : Response.Object := Response.Factory (Request);
      Call_ID           : Call_List.Identification renames
        Call_List.Value
          (Parameters (Request).Get ("call_id"));
   begin

      --  Fetch the call from the call list.
      if not Call_List.Has (ID => Call_ID) then
         Response_Object.HTTP_Status_Code (HTTP.Not_Found);
         Response_Object.Content (Status_Message ("status", "not found"));
      else

         PBX.Action.Park (Call => Call_ID,
                          User => Request_Utilities.User_Of (Request));

         --  And let the user know that everything went according to plan.
         Response_Object.HTTP_Status_Code (HTTP.OK);
         Response_Object.Content (Status_Message ("status", "request sent!"));
      end if;

      return Response_Object.Build;
   exception
      when Call_List.Invalid_ID =>
         Response_Object.HTTP_Status_Code (HTTP.Bad_Request);
         Response_Object.Content (Status_Message
                                  ("status", "bad parameter ""Call_ID"""));
         return Response_Object.Build;
      when E : others =>
         System_Message.Critical.Response_Exception
           (Event           => E,
            Message         => "Park failed",
            Response_Object => Response_Object);
         return Response_Object.Build;
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
      Response_Object   : Response.Object   := Response.Factory (Request);
      User              : Model.User.Instance
          renames Request_Utilities.User_Of (Request);
      Assigned_Call     : PBX.Call.Instance;
   begin
      if PBX.Call.List_Empty then
         Response_Object.HTTP_Status_Code (HTTP.Not_Found);
         return Response_Object.Build;
      end if;

      if not User.Peer.Registered then
         System_Messages.Error
           (Message => "User " & User.Image,
            Context => Context);
         Response_Object.HTTP_Status_Code (HTTP.Bad_Request);

         Response_Object.Content
           (Status_Message
              ("Bad request", "User has no peer unavailable"));

         return Response_Object.Build;
      else

         if Call_ID_String /= "" then
            Assigned_Call := PBX.Call.Get
              (Call => PBX.Call.Value (Call_ID_String));
         else
            Assigned_Call := PBX.Call.Highest_Prioirity;
         end if;

         Model.User.List.Get_Singleton.Assign_Call
           (User_ID => Request_Utilities.User_Of (Request).Identity,
            Call_ID => Assigned_Call.ID);

         PBX.Action.Transfer (Assigned_Call.ID, User);

         Response_Object.HTTP_Status_Code (HTTP.OK);
         Response_Object.Content
           ((To_JSON_String (Assigned_Call.To_JSON)));
         --  end if;

         return Response_Object.Build;
      end if;

      exception

         when PBX.Call.Already_Bridged =>
            Response_Object.HTTP_Status_Code (HTTP.Bad_Request);
            Response_Object.Content
              (Status_Message
                 ("Already assigned",
                  "Agent tried to claim call that is already assigned"));
            return Response_Object.Build;

      when E : others =>
         System_Message.Critical.Response_Exception
           (Event           => E,
            Message         => "Hangup failed",
            Response_Object => Response_Object);
         return Response_Object.Build;

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
      package Call_List renames PBX.Call;

      Response_Object : Response.Object := Response.Factory (Request);
   begin
      --  Only return a call list when there actual calls in it.
      if not Call_List.List_Empty then
         Response_Object.HTTP_Status_Code (HTTP.OK);
         Response_Object.Content (To_JSON_String
                                  (PBX.Call.Queued_Calls));
      else
         Response_Object.HTTP_Status_Code (HTTP.No_Content);
      end if;

      return Response_Object.Build;
   exception
      when E : others =>
         System_Message.Critical.Response_Exception
           (Event           => E,
            Message         => "call/queue failed",
            Response_Object => Response_Object);
         return Response_Object.Build;

   end Queue;

   ----------------
   --  Transfer  --
   ----------------

   function Transfer
     (Request : in AWS.Status.Data)
         return AWS.Response.Data
   is
      use PBX.Call;

      Source          : PBX.Call.Identification := Null_Identification;
      Destination     : PBX.Call.Identification := Null_Identification;
      Response_Object : Response.Object :=
        Response.Factory (Request);
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
         raise PBX.Call.Invalid_ID;
      end if;

      PBX.Action.Bridge (Source      => Source,
                         Destination => Destination);

      Response_Object.HTTP_Status_Code (HTTP.OK);
      Response_Object.Content
        (Status_Message
           ("Success", "Transfer succeeded"));

      return Response_Object.Build;

   exception
      when Invalid_ID =>
         Response_Object.HTTP_Status_Code (HTTP.Bad_Request);
         Response_Object.Content
           (Status_Message
              ("Bad request", "Invalid or no call ID supplied"));
         return Response_Object.Build;

      when PBX.Call.Not_Found =>
         Response_Object.HTTP_Status_Code (HTTP.Not_Found);
         Response_Object.Content
           (Status_Message
              ("Not found", "No call found with ID " &
                 Parameters (Request).Get ("source")));
         return Response_Object.Build;

      when E : others =>
         System_Message.Critical.Response_Exception
           (Event           => E,
            Message         => "Transfer request failed",
            Response_Object => Response_Object);
         return Response_Object.Build;
   end Transfer;

end Handlers.Call;
