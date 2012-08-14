with Ada.Containers,
     Ada.Strings.Unbounded;

with AMI.Action,
     Call_Queue,
     Call_Queue_JSON,
     Common,
     HTTP_Codes,
     Response,
     Routines;

with Yolk.Log;

with AWS.Messages;

package body Call_Queue_Handler is
   use Common;
   use AWS.Status;

   --  returns the first call in the list.
   function Get_Call (Request : in AWS.Status.Data)
                      return AWS.Response.Data is
      use Call_Queue;
      Agent : constant String := Parameters (Request).Get ("agent");
      Unitqueid : constant String :=  Parameters (Request).Get ("uniqueid");
      Call : Call_Queue.Call_Type;
      JSON : JSON_String;
      Status : Routines.Status_Type;
      Status_Code : AWS.Messages.Status_Code;
   begin
      Routines.Get_Call (Uniqueid => Unitqueid,
                         Agent    => Agent,
                         Call     => Call,
                         Status   => Status);

      case Status is
         when Routines.Success =>
            Status_Code := HTTP_Codes.OK;
            JSON := Call_Queue_JSON.Convert_Call (Call);
         when Routines.No_Call_Found =>
            Status_Code := HTTP_Codes.No_Content;
            JSON := Call_Queue_JSON.Status_Message
              ("No Cotent", "The Callqueue is empty");
         when Routines.No_Agent_Found =>
            Status_Code := HTTP_Codes.Bad_Request;
            JSON := Call_Queue_JSON.Status_Message
              ("No Parameter",
               "There exsist no agent by that name");
         when Routines.Unregistred_Agent =>
            Status_Code := HTTP_Codes.Bad_Request;
            JSON := Call_Queue_JSON.Status_Message
              ("No Cotent",
               "The agents phone is not registered");
         when others =>
            Status_Code := HTTP_Codes.Internal_Server_Error;
            JSON := Call_Queue_JSON.Status_Message
              ("Woops",
               "Something went wrong at the server");
      end case;

      return  Response.Build_JSON_Response
        (Request => Request,
         Content => JSON,
         Status  => Status_Code);
   end Get_Call;

   --  returns the number of calls waiting in the calling queue.
   function Get_Length (Request : in AWS.Status.Data)
                        return AWS.Response.Data is
      Count : Ada.Containers.Count_Type;
      JSON  : JSON_String;
   begin
      Count := Call_Queue.Queue_Length;
      --  Make the Internal calls

      JSON := Call_Queue_JSON.Convert_Length (Count);
      --  Convert it to JSON

      --  wrap it and send it.
      return  Response.Build_JSON_Response
        (Request => Request,
         Content => JSON,
         Status  => HTTP_Codes.OK);
   end Get_Length;

   --  returns the entire Call Queue, in JSON format.
   function Get_Queue (Request : in AWS.Status.Data)
                       return AWS.Response.Data is
      use Ada.Containers;

      Queue : Call_Queue.Call_Queue_Type;
      Queue_Length : Ada.Containers.Count_Type;

      JSON : JSON_String;
      Status_Code : AWS.Messages.Status_Code;
   begin
      AMI.Action.Action_Manager.Ping;

      Queue := Call_Queue.Get_Queue;
      Queue_Length := Call_Queue.Queue_Length;

      if Queue_Length /= 0 then
         JSON := Call_Queue_JSON.Convert_Queue (Queue, Queue_Length);
         Status_Code := HTTP_Codes.OK;
      else
         JSON := To_JSON_String ("{}");
         Status_Code := HTTP_Codes.No_Content;
      end if;

      return  Response.Build_JSON_Response
        (Request => Request,
         Content => JSON,
         Status  => Status_Code);
   end Get_Queue;

   function Hangup (Request : in AWS.Status.Data)
                    return AWS.Response.Data is
      use Ada.Strings.Unbounded;

      Status : Routines.Status_Type;
      JSON : JSON_String;
      Status_Code : AWS.Messages.Status_Code;
   begin
      declare
         Agent : constant String := Parameters (Request).Get ("agent");
      begin
         Yolk.Log.Trace (Yolk.Log.Debug, "Hangup handle: agent=" & Agent);
         Routines.Hangup (To_Unbounded_String (Agent), Status);
      exception
         when others =>
            Yolk.Log.Trace (Yolk.Log.Debug, "Exception in Hangup handler");
            Yolk.Log.Trace (Yolk.Log.Debug, "Exception in Hangup");
            JSON := Call_Queue_JSON.Status_Message ("Exception",
                                                    "Something went wrong");
            return  Response.Build_JSON_Response
              (Request => Request,
               Content => JSON,
               Status  => HTTP_Codes.Internal_Server_Error);
      end;

      case Status is
      when Routines.Success =>
         JSON := Call_Queue_JSON.Status_Message
           ("Success",
            "Call successfully hangup");
         Status_Code := HTTP_Codes.OK;
      when Routines.No_Agent_Found =>
         JSON := Call_Queue_JSON.Status_Message
           ("No Agent",
            "No agent was found by that name");
         Status_Code := HTTP_Codes.Bad_Request;
      when Routines.No_Call_Found =>
         JSON := Call_Queue_JSON.Status_Message
           ("No Call",
            "The agent had no call to hangup");
         Status_Code := HTTP_Codes.No_Content;
      when others =>
         JSON := Call_Queue_JSON.Status_Message
           ("Unknownen Error",
            "Something went wrong");
         Status_Code := HTTP_Codes.Bad_Request;
      end case;

      Yolk.Log.Trace
        (Yolk.Log.Debug,
         "Hangup is now returning the response. " &
           "Status code: " & Status_Code'Img & ". JSON:" &
          To_String (JSON));
      return  Response.Build_JSON_Response
        (Request => Request,
         Content => JSON,
         Status  => Status_Code);
   exception
      when others =>
         Yolk.Log.Trace (Yolk.Log.Debug, "Exception in Hangup");
         JSON := Call_Queue_JSON.Status_Message ("Exception",
                                                 "Something went wrong");
         return  Response.Build_JSON_Response
           (Request => Request,
            Content => JSON,
            Status  => HTTP_Codes.Internal_Server_Error);
   end Hangup;

   function Park_Call (Request : in AWS.Status.Data)
                       return AWS.Response.Data is
      Status_Code : AWS.Messages.Status_Code;
      Status : Routines.Status_Type;
      JSON : JSON_String;
      Call : Call_Queue.Call_Type;
   begin
      Yolk.Log.Trace (Yolk.Log.Debug, "Call_Queue_Handler.Park started");
      declare
         Agent : constant String := Parameters (Request).Get ("agent");
      begin
         Routines.Park (Agent, Call, Status);
      end;
      case Status is
         when Routines.Success =>
            Status_Code := HTTP_Codes.OK;
            JSON := Call_Queue_JSON.Convert_Call (Call);

         when Routines.No_Call_Found =>
            Status_Code := HTTP_Codes.No_Content;
            JSON := Call_Queue_JSON.Status_Message
              ("No Call",
               "There was no call to park");

         when Routines.No_Agent_Found =>
            Status_Code := HTTP_Codes.Bad_Request;
            JSON := Call_Queue_JSON.Status_Message
              ("No Agent",
               "There was no agent by that name");
         when others =>
            Status_Code := HTTP_Codes.Internal_Server_Error;
            JSON := Call_Queue_JSON.Status_Message
              ("Woops",
               "Something went wrong");
      end case;

      return  Response.Build_JSON_Response
        (Request => Request,
         Content => JSON,
         Status  => Status_Code);
   end Park_Call;

--     function Set_Call_On_Hold (Request : in AWS.Status.Data)
--                                return AWS.Response.Data is
--        Agent : constant String := Parameters (Request).Get ("agent");
--        Status : Routines.Status_Type;
--        JSON : JSON_String;
--        Status_Code : AWS.Messages.Status_Code;
--        Call : Call_Queue.Call_Type;
--     begin
--        Routines.Park (Agent, Call, Status);
--
--        case Status is
--           when Routines.Success =>
--              JSON := Call_Queue_JSON.Status_Message
--                ("success",
--                 "The call from " &
--                   Ada.Strings.Unbounded.To_String (Call.CallerIDNum) &
--                   "was placed on hold");
--              Status_Code := HTTP_Codes.OK;
--
--           when Routines.No_Call_Found =>
--              Status_Code := HTTP_Codes.Bad_Request;
--              JSON := Call_Queue_JSON.Status_Message
--                ("No Call", "The agent have no call to be parked");
--
--           when others =>
--              Status_Code := HTTP_Codes.Bad_Request;
--              JSON := Call_Queue_JSON.Status_Message
--                ("Unknownen Error",
--                 "Something unexcepted happend");
--        end case;
--
--        return  Response.Build_JSON_Response
--          (Request => Request,
--           Content => JSON,
--           Status  => Status_Code);
--     end Set_Call_On_Hold;

   function Unpark_Call (Request : in AWS.Status.Data)
                          return AWS.Response.Data is
      Agent : constant String := Parameters (Request).Get ("agent");
      Call_ID : constant String := Parameters (Request).Get ("call_id");
      Status : Routines.Status_Type;
      Status_Code : AWS.Messages.Status_Code;
      JSON : JSON_String;
   begin
      Yolk.Log.Trace (Yolk.Log.Debug, "Unpark_Call");
      Routines.UnPark (Agent   => Agent,
                       Call_ID => Call_ID,
                       Status  => Status);

      --  TODO maybe you should get the call out here.
      case Status is
         when Routines.Success =>
            Status_Code := HTTP_Codes.OK;
            JSON := Call_Queue_JSON.Status_Message
              ("Success",
               "No problem, the call was undirected");
         when Routines.No_Agent_Found =>
            Status_Code := HTTP_Codes.Bad_Request;
            JSON := Call_Queue_JSON.Status_Message
              ("No Agent",
               "There was no Agent by that name");
         when Routines.No_Call_Found =>
            Status_Code := HTTP_Codes.Bad_Request;
            JSON := Call_Queue_JSON.Status_Message
              ("No Call",
               "This is not the call you are looking for");
         when others =>
            Status_Code := HTTP_Codes.Bad_Request;
            JSON := Call_Queue_JSON.Status_Message
              ("Unknonen Error",
               "There happen to be an unknownen error");
      end case;

      return  Response.Build_JSON_Response
        (Request => Request,
         Content => JSON,
         Status  => Status_Code);
   end Unpark_Call;
end Call_Queue_Handler;
