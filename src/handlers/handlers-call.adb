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
with AWS.Messages;
with Common;
with HTTP_Codes;
with Response;

with AMI.Action;
with Model.Agents;
with Model.Agent_ID;
with Model.Call;
with Model.Calls;
with Model.Peers;
with JSON.Call;

with PBX;
with Model.Call_ID;
with System_Messages;

package body Handlers.Call is
   use System_Messages;
   use AMI.Action;
   use JSON.Call;
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
      use HTTP_Codes;
      use AWS.Status;
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
      use HTTP_Codes;
      use AWS.Status;
      use Model.Call;
      use Model.Calls;
      use Model.Call_ID;

      Context         : constant String := "Handlers.Call.Hold";
      Response_Object : Response.Object := Response.Factory (Request);
      Call_ID         : constant Call_ID_Type :=
        Create (Item => Parameters (Request).Get ("Call_ID"));
      Call            : constant Call_Type := Get (Call_ID);
   begin
      System_Messages.Notify
        (Debug, "Park: Action send, Call_ID => " & Call_ID.To_String);

      Routines.Park (PBX.Client_Access, Call);

      Response_Object.HTTP_Status_Code (OK);
      Response_Object.Content
        (Status_Message ("Success", "Request received"));

      return Response_Object.Build;
   exception
      when CALL_NOT_FOUND =>
         System_Messages.Notify
           (Debug, Context & ": Call not found, ID: " & Call_ID.To_String);
         Response_Object.HTTP_Status_Code (Bad_Request);
         Response_Object.Content
           (Status_Message
              ("bad request", "No call available to put on hold"));
         return Response_Object.Build;
      when others =>
         Response_Object.HTTP_Status_Code (Server_Error);
         Response_Object.Content
           (Status_Message
              ("oops", "Stuff went wrong :-("));
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
      use HTTP_Codes;

      Response_Object : Response.Object := Response.Factory (Request);
   begin
      Response_Object.HTTP_Status_Code (OK);
      Response_Object.Content
        (To_JSON_String (Calls.List.To_JSON.Write));

      return Response_Object.Build;
   end List;

   --------------
   --  Pickup  --
   --------------

   function Pickup
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use AWS.Status;
      use Model.Call;
      use Model.Calls;
      use Model.Peers;
      use Common;
      use HTTP_Codes;
      use Ada.Strings.Unbounded;
      use Model.Call_ID;
      use Model.Agent_ID;

      Agent_ID : constant Agent_ID_Type :=
                   Create (Parameters (Request).Get ("agent_id"));
      Call_ID  : constant String := Parameters (Request).Get ("call_id");

      Content         : JSON_String;
      Response_Object : Response.Object := Response.Factory (Request);
      Status_Code     : AWS.Messages.Status_Code;
      Call            : Call_Type := Null_Call;
      Peer            : Peer_Type;
   begin

      --  Retrieve the peer from the agent_id.
      Peer := Model.Peers.List.Get
        (Model.Agents.Get (Agent_ID => Agent_ID).Peer_ID);

      --  Determine which call to pickup
      if Call_ID'Length = 0 then
         System_Messages.Notify (Debug, "Get_Call - Agent_ID: " &
                                   Parameters (Request).Get ("agent_id") &
                                   " asks for unspecifed call");
         Call := Calls.List.Next;
         System_Messages.Notify (Debug, " Sending: " & To_String (Call));
      else
         System_Messages.Notify (Debug, "Get_Call - Agent_ID: " &
                                   Parameters (Request).Get ("agent_id") &
                                   " ask for Call_ID: " & Call_ID);
         --  Lookup the call
         Call := Calls.Dequeue (Create (Call_ID));
      end if;

      if Call = Null_Call then
         System_Messages.Notify
           (Debug, "Get_Call: No Call to take with ID: " & Call_ID);
         Status_Code := No_Content;
         Content := Status_Message
           ("Bad request", "No call found");
      elsif Peer.State = Unregistered then
         System_Messages.Notify
           (Critical, "Get_Call: " &
              "The following agent is unregistred: " &
              Parameters (Request).Get ("agent_id"));
         Status_Code := Bad_Request;
         Content := Status_Message
           ("Bad request", "The agents phone is not registered");
      else
         --  Send the command to AMI
         AMI.Action.Redirect (Client    => PBX.Client_Access,
                              Channel   => Call.Channel_ID,
                              Extension => "101"); --  To_String (Peer.Exten)
         Status_Code := OK;
         Content := To_JSON_String (Call);
         --  (Status_Message ("Success", "The request is being processed"));
      end if;

      Response_Object.HTTP_Status_Code (Status_Code);
      Response_Object.Content (Content);

      return Response_Object.Build;

   exception
      when PEER_NOT_FOUND =>
         Response_Object.HTTP_Status_Code (Bad_Request);
         Response_Object.Content
           (Status_Message
              ("No Parameter", "There exsist no agent by that name"));
         return Response_Object.Build;

      when CALL_NOT_FOUND =>
         Response_Object.HTTP_Status_Code (No_Content);
         return Response_Object.Build;

      when others =>
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
      use HTTP_Codes;

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
