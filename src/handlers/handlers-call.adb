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

with AWS.Messages;
with Common;
with HTTP_Codes;
with Response;

with AMI.Action;

with Model.Call;
with JSON.Call;

with PBX;

with System_Messages;

package body Handlers.Call is
   use System_Messages;
   use AMI.Action;
   use JSON.Call;
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

      JSON            : JSON_String;
      Response_Object : Response.Object := Response.Factory (Request);
      Status          : Routines.Status_Type;
      --  ???? Perhaps renaming Status to Call_Status, PBX_Status or something
      --  else would make things more clear? It clashes somewhat with the
      --  HTTP request Status_Code.
      Status_Code     : AWS.Messages.Status_Code;
   begin
      --   ???? Why do we have a block here? All it seems to do is catch
      --  "unknown" exceptions. Can't these just as well be caught in the main
      --  block?
      declare
         use AWS.Status;

         Call_ID : constant String := Parameters (Request).Get ("Call_id");
         --  ???? Agent? This constant is used with the Routines.Hangup
         --  procedure which takes a Call_Id. Bug or by design?
      begin
         System_Messages.Notify (Debug, "Hangup handle: call_id=" & Call_ID);
         AMI.Action.Hangup (PBX.Client_Access, U (Call_ID), Status);
         --  ???? Why the conversion to Unbounded_String here? In most of the
         --  other Routines methods you take a plain String and convert in the
         --  method instead.
      exception
         when others =>
            System_Messages.Notify
              (Debug, "Exception in Call_Queue.Call_Hangup");

            Response_Object.Set_HTTP_Status_Code (Server_Error);
            Response_Object.Set_Content
              (Status_Message
                 ("Exception", "Something went wrong"));

            return Response_Object.Build;
      end;

      --  ???? It seems to me that this case block is more or less repeated
      --  verbatim in this package. Perhaps it could/should be sourced into
      --  its own local method?
      case Status is
         when Routines.Success =>
            JSON := Status_Message
              ("Success", "Hangup completed");
            Status_Code := OK;
         when Routines.No_Agent_Found =>
            JSON := Status_Message
              ("No Agent",
               "No agent was found by that name");
            Status_Code := Bad_Request;
         when Routines.No_Call_Found =>
            JSON := Status_Message
              ("No Call",
               "The agent had no call to hangup");
            Status_Code := No_Content;
         when others =>
            JSON := Status_Message
              ("Unknownen Error",
               "Something went wrong");
            Status_Code := Bad_Request;
            --  ???? Shouldn't this be Server_Error?
      end case;

      System_Messages.Notify (Debug,
             "Hangup is now returning the response. "
             & "Status code: " & Status_Code'Img & ". JSON:"
             & To_String (JSON));

      Response_Object.Set_HTTP_Status_Code (Status_Code);
      Response_Object.Set_Content (JSON);

      return Response_Object.Build;
   exception
      when others =>
         --  ???? What exceptions are we expecting, and why do we not catch
         --  exceptions in any of the other methods in this package?
         System_Messages.Notify (Debug, "Exception in Hangup");

         Response_Object.Set_HTTP_Status_Code (Server_Error);
         Response_Object.Set_Content
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

      JSON            : JSON_String;
      Response_Object : Response.Object := Response.Factory (Request);
      Status          : Routines.Status_Type;
      Status_Code     : AWS.Messages.Status_Code;

      Call_Id : constant String := Parameters (Request).Get ("Call_ID");
   begin
      System_Messages.Notify (Debug, "Call_Queue_Handler.Park started");

      Routines.Park (PBX.Client_Access, Call_Id, Status);

      case Status is
         when Routines.Success =>
            Status_Code := OK;
            JSON := Status_Message
              ("Success",
               "The command is being process");

         when Routines.No_Call_Found =>
            Status_Code := No_Content;
            JSON := Status_Message
              ("No Call",
               "There was no call to park");

         when Routines.No_Agent_Found =>
            Status_Code := Bad_Request;
            JSON := Status_Message
              ("No Agent",
               "There was no agent by that name");
         when others =>
            Status_Code := Server_Error;
            JSON := Status_Message
              ("Woops",
               "Something went wrong");
      end case;

      Response_Object.Set_HTTP_Status_Code (Status_Code);
      Response_Object.Set_Content (JSON);

      return Response_Object.Build;
      --  ???? No exception handler? Is the caller of Call_Hold expected to
      --  deal with unhandled issues happening in for example Routines.Park,
      --  Parameters, Call_Queue_JSON.Status_Message or Build_JSON_Response?
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

      JSON            : JSON_String;
      Response_Object : Response.Object := Response.Factory (Request);
      --  ???? Odd naming. See ???? comment for Call_List.Call_List_Type.
   begin
      --  ???? If we're ultimately just interested in getting a queue JSON
      --  document, be it empty or filled with calls, why go through all the
      --  hassle of getting a Call_List_Type.Vector? Do we need this here?
      --  Why not just have a function in the Call_List package that return
      --  the final JSON and use that directly in the Build_JSON_Response call?

      JSON := To_JSON_String (Model.Call.Get);

      Response_Object.Set_HTTP_Status_Code (OK);
      Response_Object.Set_Content (JSON);

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
      use Common;
      use HTTP_Codes;

      Agent       : constant String := Parameters (Request).Get ("agent");
      Unique_Id   : constant String := Parameters (Request).Get ("uniqueid");

      JSON            : JSON_String;
      Response_Object : Response.Object := Response.Factory (Request);
      Status          : Routines.Status_Type;
      Status_Code     : AWS.Messages.Status_Code;
   begin
      Routines.Get_Call (Client      => PBX.Client_Access,
                         Unique_Id   => Unique_Id,
                         Agent_Id    => Agent,
                         --  Call     => Call,
                         Status      => Status);
      case Status is
         when Routines.Success =>
            Status_Code := OK;
            JSON := Status_Message
              ("Success", "The request is being processed");
         when Routines.No_Call_Found =>
            Status_Code := No_Content;
            JSON := Status_Message
              ("No Cotent", "The Callqueue is empty");
         when Routines.No_Agent_Found =>
            Status_Code := Bad_Request;
            JSON := Status_Message
              ("No Parameter",
               "There exsist no agent by that name");
         when Routines.Unregistered_Agent =>
            Status_Code := Bad_Request;
            JSON := Status_Message
              ("No Cotent",
               "The agents phone is not registered");
         when others =>
            Status_Code := Server_Error;
            JSON := Status_Message
              ("Woops",
               "Something went wrong at the server");
      end case;

      Response_Object.Set_HTTP_Status_Code (Status_Code);
      Response_Object.Set_Content (JSON);

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
      Response_Object.Set_HTTP_Status_Code (OK);
      Response_Object.Set_Content (To_JSON_String (Model.Call.Get));

      return Response_Object.Build;
   end Queue;

end Handlers.Call;
