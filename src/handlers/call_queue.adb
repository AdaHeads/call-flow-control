-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                               Call_Queue                                  --
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

with Ada.Containers;
with AWS.Messages;
with Call_List;
with Call_Queue_JSON;
with Common;
with HTTP_Codes;
with Response;
with Routines;
with Yolk.Log;
with Yolk.Utilities;

package body Call_Queue is

   -------------------
   --  Call_Hangup  --
   -------------------

   function Call_Hangup
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use Common;
      use HTTP_Codes;
      use Yolk.Log;
      use Yolk.Utilities;

      JSON        : JSON_String;
      Status      : Routines.Status_Type;
      --  ???? Perhaps renaming Status to Call_Status, PBX_Status or something
      --  else would make things more clear? It clashes somewhat with the
      --  HTTP request Status_Code.
      Status_Code : AWS.Messages.Status_Code;
   begin
      --   ???? Why do we have a block here? All it seems to do is catch
      --  "unknown" exceptions. Can't these just as well be caught in the main
      --  block?
      declare
         use AWS.Status;

         Agent : constant String := Parameters (Request).Get ("agent");
         --  ???? Agent? This constant is used with the Routines.Hangup
         --  procedure which takes a Call_Id. Bug or by design?
      begin
         Trace (Debug, "Hangup handle: agent=" & Agent);
         Routines.Hangup (TUS (Agent), Status);
         --  ???? Why the conversion to Unbounded_String here? In most of the
         --  other Routines methods you take a plain String and convert in the
         --  method instead.
      exception
         when others =>
            Trace (Debug, "Exception in Call_Queue.Call_Hangup");

            JSON := Call_Queue_JSON.Status_Message ("Exception",
                                                    "Something went wrong");
            --  ???? Why not use the Error.Exception_Handler function instead?

            return Response.Build_JSON_Response
              (Request => Request,
               Content => JSON,
               Status  => Server_Error);
      end;

      --  ???? It seems to me that this case block is more or less repeated
      --  verbatim in this package. Perhaps it could/should be sourced into
      --  its own local method?
      case Status is
         when Routines.Success =>
            JSON := Call_Queue_JSON.Status_Message
              ("Success", "Hangup completed");
            Status_Code := OK;
         when Routines.No_Agent_Found =>
            JSON := Call_Queue_JSON.Status_Message
              ("No Agent",
               "No agent was found by that name");
            Status_Code := Bad_Request;
         when Routines.No_Call_Found =>
            JSON := Call_Queue_JSON.Status_Message
              ("No Call",
               "The agent had no call to hangup");
            Status_Code := No_Content;
         when others =>
            JSON := Call_Queue_JSON.Status_Message
              ("Unknownen Error",
               "Something went wrong");
            Status_Code := Bad_Request;
            --  ???? Shouldn't this be Server_Error?
      end case;

      Trace (Debug,
             "Hangup is now returning the response. "
             & "Status code: " & Status_Code'Img & ". JSON:"
             & To_String (JSON));

      return  Response.Build_JSON_Response
        (Request => Request,
         Content => JSON,
         Status  => Status_Code);
   exception
      when others =>
         --  ???? What exceptions are we expecting, and why do we not catch
         --  exceptions in any of the other methods in this package?
         Trace (Debug, "Exception in Hangup");
         JSON := Call_Queue_JSON.Status_Message ("Exception",
                                                 "Something went wrong");

         return  Response.Build_JSON_Response
           (Request => Request,
            Content => JSON,
            Status  => Server_Error);
   end Call_Hangup;

   -----------------
   --  Call_Hold  --
   -----------------

   function Call_Hold
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use Common;
      use HTTP_Codes;
      use Yolk.Log;

      JSON        : JSON_String;
      Status      : Routines.Status_Type;
      Status_Code : AWS.Messages.Status_Code;
   begin
      Trace (Debug, "Call_Queue_Handler.Park started");

      --   ???? Why this block?
      declare
         use AWS.Status;

         Call_Id : constant String := Parameters (Request).Get ("Call_ID");
      begin
         Routines.Park (Call_Id, Status);
      end;

      case Status is
         when Routines.Success =>
            Status_Code := OK;
            JSON := Call_Queue_JSON.Status_Message
              ("Success",
               "The command is being process");

         when Routines.No_Call_Found =>
            Status_Code := No_Content;
            JSON := Call_Queue_JSON.Status_Message
              ("No Call",
               "There was no call to park");

         when Routines.No_Agent_Found =>
            Status_Code := Bad_Request;
            JSON := Call_Queue_JSON.Status_Message
              ("No Agent",
               "There was no agent by that name");
         when others =>
            Status_Code := Server_Error;
            JSON := Call_Queue_JSON.Status_Message
              ("Woops",
               "Something went wrong");
      end case;

      return  Response.Build_JSON_Response
        (Request => Request,
         Content => JSON,
         Status  => Status_Code);
      --  ???? No exception handler? Is the caller of Call_Hold expected to
      --  deal with unhandled issues happening in for example Routines.Park,
      --  Parameters, Call_Queue_JSON.Status_Message or Build_JSON_Response?
   end Call_Hold;

   -------------------
   --  Call_Pickup  --
   -------------------

   function Call_Pickup
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use AWS.Status;
      use Call_List;
      use Common;
      use HTTP_Codes;

      Agent       : constant String := Parameters (Request).Get ("agent");
      Unique_Id   : constant String := Parameters (Request).Get ("uniqueid");

      JSON        : JSON_String;
      Status      : Routines.Status_Type;
      Status_Code : AWS.Messages.Status_Code;
   begin
      Routines.Get_Call (Unique_Id   => Unique_Id,
                         Agent_Id    => Agent,
                         --  Call     => Call,
                         Status      => Status);

      case Status is
         when Routines.Success =>
            Status_Code := OK;
            JSON := Call_Queue_JSON.Status_Message
              ("Success", "The request is being processed");
         when Routines.No_Call_Found =>
            Status_Code := No_Content;
            JSON := Call_Queue_JSON.Status_Message
              ("No Cotent", "The Callqueue is empty");
         when Routines.No_Agent_Found =>
            Status_Code := Bad_Request;
            JSON := Call_Queue_JSON.Status_Message
              ("No Parameter",
               "There exsist no agent by that name");
         when Routines.Unregistered_Agent =>
            Status_Code := Bad_Request;
            JSON := Call_Queue_JSON.Status_Message
              ("No Cotent",
               "The agents phone is not registered");
         when others =>
            Status_Code := Server_Error;
            JSON := Call_Queue_JSON.Status_Message
              ("Woops",
               "Something went wrong at the server");
      end case;

      return  Response.Build_JSON_Response
        (Request => Request,
         Content => JSON,
         Status  => Status_Code);
   end Call_Pickup;

   -----------------
   --  Get_Queue  --
   -----------------

   function Get_Queue
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use Ada.Containers;
      use Common;
      use HTTP_Codes;

      JSON         : JSON_String;
      Queue        : Call_List.Call_List_Type.Vector;
      --  ???? Odd naming. See ???? comment for Call_List.Call_List_Type.
      Queue_Length : Count_Type;
      Status_Code  : AWS.Messages.Status_Code;
   begin
      --  ???? If we're ultimately just interested in getting a queue JSON
      --  document, be it empty or filled with calls, why go through all the
      --  hassle of getting a Call_List_Type.Vector? Do we need this here?
      --  Why not just have a function in the Call_List package that return
      --  the final JSON and use that directly in the Build_JSON_Response call?
      Queue := Call_List.Get;
      Queue_Length := Call_List.Length;

      if Queue_Length /= 0 then
         JSON := Call_Queue_JSON.Convert_Queue (Queue);
         Status_Code := OK;
      else
         JSON := To_JSON_String ("{}");
         --  ???? The GNATCOLL.JSON.Create_Object call is perhaps a more clean
         --  way of getting an empty JSON document.
         Status_Code := No_Content;
         --  ???? No_Content makes no sense here, because you've just built
         --  the simple content {}
         --  If the call to Get_Queue is successful, you should probably just
         --  set OK. The only reason for setting a different HTTP code is if
         --  something really awkward has happened.
      end if;

      return  Response.Build_JSON_Response
        (Request => Request,
         Content => JSON,
         Status  => Status_Code);
   end Get_Queue;

   --------------
   --  Length  --
   --------------

   function Length
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use Ada.Containers;
      use Common;
      use HTTP_Codes;

      Count : Count_Type;
      JSON  : JSON_String;
   begin
      Count := Call_List.Length;
      --  Make the Internal calls

      JSON := Call_Queue_JSON.Convert_Length (Count);
      --  Convert it to JSON

      --  ???? Why the need for a conversion step here? Wouldn't this be more
      --  cleanly handled in Call_List.Length using an overloaded Length
      --  function?

      return  Response.Build_JSON_Response
        (Request => Request,
         Content => JSON,
         Status  => OK);
   end Length;

end Call_Queue;
