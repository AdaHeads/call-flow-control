-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                              Handlers.Call                                --
--                                                                           --
--                                  BODY                                     --
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

with AMI.Action;
with Model.Agent;
with Model.Agents;
with Model.Agent_ID;
with Model.Call;
with Model.Calls;
with Model.Peers;
with View.Call;

with PBX;
with Model.Call_ID;
with System_Messages;
with System_Message.Critical;

package body Handlers.Call is
   use AWS.Status;
   use HTTP_Codes;
   use System_Messages;
   use AMI.Action;
   use View.Call;
   use Model;

   package Routines renames AMI.Action;

   --------------
   --  Hangup  --
   --------------

   function Hangup
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use Common;
      use Model.Call_ID;
      use Model.Call;
      use Model.Calls;

      Call_ID         : constant Call_ID_Type :=
        Create (Item => Parameters (Request).Get ("Call_ID"));
      Response_Object : Response.Object := Response.Factory (Request);
   begin
      System_Messages.Notify
        (Debug, "Hangup handle: call_id=" & Call_ID.To_String);
      AMI.Action.Hangup (PBX.Client_Access, Call_ID);

      Response_Object.HTTP_Status_Code (OK);
      Response_Object.Content (Status_Message
              ("Success", "Hangup completed"));

      if not Calls.List.Contains (Call_ID => Call_ID) then
         Response_Object.HTTP_Status_Code (Not_Found);
         Response_Object.Content
              (Status_Message
                 ("status", "not found"));
         return Response_Object.Build;
      end if;

      return Response_Object.Build;

   exception
      when others =>
         --  ???? What exceptions are we expecting, and why do we not catch
         --  exceptions in any of the other methods in this package?
         System_Messages.Notify (Debug, "Exception in Hangup");
         Response_Object.HTTP_Status_Code (Server_Error);
         Response_Object.Content
           (Status_Message
              ("Exception", "Something went wrong"));

         return Response_Object.Build;
   end Hangup;

   ------------
   --  Hold  --
   ------------

   function Hold
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use Common;
      use Model.Call;
      use Model.Calls;
      use Model.Call_ID;

      Context         : constant String := "Hold";
      Response_Object : Response.Object := Response.Factory (Request);
      Call_ID         : constant Call_ID_Type :=
                          Create
                            (Item => Parameters (Request).Get ("call_id"));
      Call            : Call_Type := Null_Call;

   begin
      Call := Model.Calls.List.Get (Call_ID);

      if Call = Null_Call then
         System_Messages.Notify
           (Debug, Context & ": Call not found, ID: " & Call_ID.To_String);
         Response_Object.HTTP_Status_Code (Bad_Request);
         Response_Object.Content
           (Status_Message
              ("bad request", "No call available to put on hold"));
         return Response_Object.Build;
      end if;

      System_Messages.Notify
        (Debug, "Park: Action send, Call_ID => " & Call_ID.To_String);

      Routines.Park (PBX.Client_Access, Call.Channel_ID);

      Response_Object.HTTP_Status_Code (OK);
      Response_Object.Content
        (Status_Message ("success", "request received"));

      return Response_Object.Build;
   exception
      when E : others =>
         System_Message.Critical.Response_Exception
           (Event           => E,
            Message         => "Hold failed",
            Response_Object => Response_Object);
         return Response_Object.Build;
   end Hold;

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
      Response_Object.HTTP_Status_Code (OK);
      Response_Object.Content
        (To_JSON_String (Calls.List.To_JSON.Write));

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
      use Model.Agent;

      Agent_ID_String  : constant String :=
                          Parameters (Request).Get ("agent_id");
      Extension_String : constant String :=
                           Parameters (Request).Get ("extension");
      Agent            : Model.Agent.Agent_Type := Null_Agent;
      Response_Object  : Response.Object := Response.Factory (Request);

   begin
      Agent := Model.Agents.Get (Model.Agent_ID.Create (Agent_ID_String));

      AMI.Action.Originate (Client    => PBX.Client_Access,
                            Peer_ID   => Agent.Peer_ID,
                            Context   => "LocalSets",
                            Extension => Extension_String,
                            Priority  => 1);

      Response_Object.HTTP_Status_Code (OK);
      Response_Object.Content (Status_Message
                        ("status", "ok"));
      return Response_Object.Build;
   end Originate;

   ------------
   --  Park  --
   ------------

   function Park (Request : in AWS.Status.Data)
                  return AWS.Response.Data is
      use Model.Call;

      Response_Object  : Response.Object := Response.Factory (Request);
      Call             : constant Call_Type :=
                           Model.Calls.List.Get
                             (Model.Call_ID.Create
                                (Parameters (Request).Get ("call_id")));
   begin
      AMI.Action.Park (Client      => PBX.Client_Access,
                       Channel_ID  => Call.Channel_ID);

      Response_Object.HTTP_Status_Code (OK);
      Response_Object.Content (Status_Message ("status", "ok"));

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

      Call_ID_Request   : constant String :=
                            Parameters (Request).Get (Name => "call_id");
      Agent_ID_String   : constant String :=
                            Parameters (Request).Get (Name => "agent_id");
      Response_Object   : Response.Object := Response.Factory (Request);
      Agent_ID          : Agent_ID_Type   := Null_Agent_ID;
      Agent             : Agent_Type      := Null_Agent;
      Requested_Call    : Call_Type       := Null_Call;
      Requested_Call_ID : Call_ID_Type    := Null_Call_ID;
      Peer              : Peer_Type       := Null_Peer;
   begin
      --  We want a valid agent ID, so we let the exception propogate.
      Agent_ID := Create (Agent_ID => Agent_ID_String);

      --  No call id is supplied, just give the client the next call.
      if Parameters (Request).Exist ("call_id") then
         if not Model.Call_ID.Validate (Call_ID_Request) then
            Response_Object.HTTP_Status_Code (Bad_Request);
            Response_Object.Content
              (Status_Message
                 ("bad request", "invalid call id"));

            return Response_Object.Build;
         end if;

         --  Turn the string into a call ID
         Requested_Call_ID := Create (Parameters (Request).Get ("call_id"));
      end if;

      --  Retrieve the call
      Calls.List.Dequeue (Requested_Call_ID, Requested_Call);

      --  If we did not claim a call at this point, return HTTP 204.
      if Requested_Call = Null_Call then
         System_Messages.Notify
           (Debug, "Get_Call: No Call to take with ID: " &
              Requested_Call_ID.To_String);
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
         Agent.Assign (Call => Requested_Call);

         --  Set the call state.
         Requested_Call.State := Speaking;

         --  Put back the call into the call list
         Model.Calls.List.Insert (Call =>  Requested_Call);

         --  Send the command to AMI
         AMI.Action.Redirect
           (Client    => PBX.Client_Access,
            Channel   => Requested_Call.Channel_ID,
            Extension => Agent.Extension);
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

   function Queue
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use Common;

      Response_Object : Response.Object := Response.Factory (Request);
   begin
      --  ???? If we're ultimately just interested in getting a queue JSON
      --  document, be it empty or filled with calls, why go through all the
      --  hassle of getting a Call_List_Type.Vector? Do we need this here?
      --  Why not just have a function in the Call_List package that return
      --  the final JSON and use that directly in the Build_JSON_Response call?

      Response_Object.HTTP_Status_Code (OK);
      Response_Object.Content
        (Common.To_JSON_String (Calls.List.To_JSON.Write));

      return Response_Object.Build;
   end Queue;

end Handlers.Call;
