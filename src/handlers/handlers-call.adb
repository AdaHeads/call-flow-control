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
with Ada.Exceptions;
with Common;
with HTTP_Codes;
with Response;

with AMI;
with AMI.Action;
with Model.Agent;
with Model.Agents;
with Model.Agent_ID;
with Model.Call;
with Model.Calls;
with Model.Peers;
with View.Call;

with PBX;
with PBX.Action;
with Model.Call_ID;
with System_Messages;
with System_Message.Critical;
with System_Message.Error;

package body Handlers.Call is
   use AWS.Status;
   use HTTP_Codes;
   use System_Messages;
   use AMI.Action;
   use View.Call;
   use Model;

   --------------
   --  Hangup  --
   --------------

   --  TODO: Add check for valid agent.
   function Hangup
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use Common;

      Requested_Call_ID : Call_ID.Call_ID_Type := Call_ID.Null_Call_ID;
      Response_Object   : Response.Object := Response.Factory (Request);
      Ticket            : PBX.Reply_Ticket := PBX.Null_Reply;
   begin
      Requested_Call_ID :=
        Call_ID.Create (Item => Parameters (Request).Get ("call_id"));
      --  Will raise exception, if ID is not valid.

      if not Calls.List.Contains (Call_ID => Requested_Call_ID) then
         Response_Object.HTTP_Status_Code (Not_Found);
         Response_Object.Content
              (Status_Message
                 ("status", "not found"));
      else
         Ticket := PBX.Action.Hangup (ID => Requested_Call_ID);
      end if;

      PBX.Action.Wait_For (Ticket);

      Response_Object.HTTP_Status_Code (OK);
      Response_Object.Content (Status_Message
                               ("Success", "Hangup completed"));


      return Response_Object.Build;

   exception
      when E : Call_ID.Invalid_ID =>
         System_Message.Error.Bad_Call_ID_Key
           (Event           => E,
            Message         => "Could not lookup call",
            Response_Object => Response_Object);
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
      use Common;

      Response_Object : Response.Object := Response.Factory (Request);
   begin
      --  Only return a call list when there actual calls in it.
      if not Calls.List.Is_Empty then
         Response_Object.HTTP_Status_Code (OK);
         Response_Object.Content
           (To_JSON_String (Calls.List.To_JSON.Write));
      else
         Response_Object.HTTP_Status_Code (No_Content);
      end if;

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

   --  /call/originate?[agent_id=<agent_id>|cm_id=<cm_id>|pstn_number=
   --  <Pstn_NumbEr >  | sip =  < sip_uri > ]
   function Originate
     (Request : in AWS.Status.Data)
      return AWS.Response.Data is
      use Common;

      Agent_ID_String   : constant String :=
                            Parameters (Request).Get ("agent_id");
      Extension_String  : constant String :=
                            Parameters (Request).Get ("extension");
      Originating_Agent : Model.Agent.Agent_Type := Agent.Null_Agent;
      Response_Object   : Response.Object := Response.Factory (Request);

   begin
      Originating_Agent :=
        Model.Agents.Get (Model.Agent_ID.Create (Agent_ID_String));

      AMI.Action.Originate (Client    => PBX.Client_Access,
                            Peer_ID   => Originating_Agent.Peer_ID,
                            Context   => Originating_Agent.Context,
                            Extension => Extension_String,
                            Priority  => 1);

      Response_Object.HTTP_Status_Code (OK);
      Response_Object.Content (Status_Message
                        ("status", "ok"));
      return Response_Object.Build;
   exception
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

      Response_Object   : Response.Object      := Response.Factory (Request);
      Requested_Call    : Model.Call.Call_Type       := Model.Call.Null_Call;
      Call_ID_Parameter : constant String :=
                            Parameters (Request).Get ("call_id");
   begin
      --  Fetch the call from the call list.
      if not Calls.List.Contains (Call_ID.Create (Call_ID_Parameter)) then
         Response_Object.HTTP_Status_Code (Not_Found);
         Response_Object.Content (Status_Message ("status", "not found"));
      else
         --  Fetch the call
         Requested_Call := Calls.List.Get
           (Call_ID.Create (Call_ID_Parameter));

         --  Park it
         AMI.Action.Park (Client      => PBX.Client_Access,
                          Channel  => Requested_Call.Channel_ID.To_String,
                          Fallback_Channel => "");

         --  And let the user know that everything went according to plan.
         Response_Object.HTTP_Status_Code (OK);
         Response_Object.Content (Status_Message ("status", "ok"));
      end if;

      return Response_Object.Build;
   exception
      when Call_ID.Invalid_ID =>
         Response_Object.HTTP_Status_Code (Bad_Request);
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
      use Common;

      use Model.Agent_ID;
      use Model.Agent;
      use Model.Call;
      use Model.Call_ID;
      use Model.Peers;

      Call_ID_String    : constant String :=
                            Parameters (Request).Get (Name => "call_id");
      Agent_ID_String   : constant String :=
                            Parameters (Request).Get (Name => "agent_id");
      Response_Object   : Response.Object := Response.Factory (Request);
      Agent_ID          : Agent_ID_Type   := Null_Agent_ID;
      Agent             : Agent_Type      := Null_Agent;
      Requested_Call    : Call_Type       := Null_Call;
      Peer              : Peer_Type       := Null_Peer;
   begin
      --  We want a valid agent ID, so we let the exception propogate.
      Agent_ID := Create (Agent_ID => Agent_ID_String);

      --  Check valitity of the call. (Will raise exception on invalid.
      if Parameters (Request).Exist ("call_id") then
         Requested_Call := Calls.List.Get
           (Call_ID => Model.Call_ID.Create (Call_ID_String));
      end if;

      --  If we do not have any calls at this point, return HTTP 204.
      if Calls.List.Is_Empty then
         Response_Object.HTTP_Status_Code (No_Content);
         return Response_Object.Build;
      end if;

      --  Lookup the peer from the agent_id.
      Peer := Model.Peers.List.Get
        (Model.Agents.Get (Agent_ID => Agent_ID).Peer_ID);

      if not Peer.Available then
         System_Messages.Notify
           (Critical, "Get_Call: " &
              "The following agent is unavailable: " &
              Agent_ID.To_String);
         Response_Object.HTTP_Status_Code (Bad_Request);
         Response_Object.Content
           (Status_Message
              ("Bad request", "Agent peer unavailable"));

         return Response_Object.Build;
      else
         --  We're good - transfer the call.
         Agent := Model.Agents.Get (Agent_ID => Agent_ID);
         Calls.List.Assign
           (Agent => Agent,
            Call  => Requested_Call);

         --  Send the command to AMI
         AMI.Action.Redirect
           (Client    => PBX.Client_Access,
            Channel   => Requested_Call.Channel_ID,
            Extension => Agent.Extension,
            Context   => "LocalSets");
         Response_Object.HTTP_Status_Code (OK);
         Response_Object.Content
           ((To_JSON_String (Requested_Call.To_JSON.Write)));
      end if;

      return Response_Object.Build;

   exception
      when E : Model.Agent_ID.Invalid_ID =>
         System_Messages.Notify
           (Error, Ada.Exceptions.Exception_Information (E));
         Response_Object.HTTP_Status_Code (Server_Error);
         Response_Object.Content
           (Status_Message
              ("Uh-oh", "You don't seem to have a valid agent ID"));
         return Response_Object.Build;

      when Calls.Already_Assigned =>
         Response_Object.HTTP_Status_Code (Bad_Request);
         Response_Object.Content
           (Status_Message
              ("Already assigned",
               "Agent tried to claim call that is already assigned"));
         return Response_Object.Build;

      when Peer_Not_Found =>
         Response_Object.HTTP_Status_Code (Bad_Request);
         Response_Object.Content
           (Status_Message
              ("No Parameter", "There exsist no agent by that name"));
         return Response_Object.Build;

      when E : others =>
         System_Messages.Notify
           (Error, Ada.Exceptions.Exception_Information (E));
         Response_Object.HTTP_Status_Code (Server_Error);
         Response_Object.Content
           (Status_Message
              ("Woops", "Something went wrong at the server"));
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
      use Common;

      --  Response_Object : Response.Object := Response.Factory (Request);
   begin

      return List (Request);
   end Queue;

end Handlers.Call;
