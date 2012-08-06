with Ada.Containers,
     Ada.Strings.Unbounded;

with AMI.Action,
     Call_Queue,
     Call_Queue_JSON,
     Common,
     HTTP_Codes,
     Response,
     Routines;

package body Call_Queue_Handler is
   use Common;

   --  returns the first call in the list.
   function Get_Call (Request : in AWS.Status.Data)
                      return AWS.Response.Data is
      use AWS.Status;
      use Call_Queue;
      Agent : constant String := Parameters (Request).Get ("agent");
      Unitqueid : constant String :=  Parameters (Request).Get ("uniqueid");
      Call : Call_Queue.Call_Type;
      JSON : JSON_String;
      Status : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Routines.Get_Call (Uniqueid => Unitqueid,
                         Agent    => Agent,
                         Call     => Call,
                         Status   => Status);
      if Call = Call_Queue.null_Call then
         Routines.TEST_StatusPrint;
         JSON := To_JSON_String ("{ ""Status"" : """ &
                                   Ada.Strings.Unbounded.To_String (Status)
                                 & """}");
      else
         JSON := Call_Queue_JSON.Convert_Call (Call);
      end if;

      return  Response.Build_JSON_Response
        (Request => Request,
         Content => JSON,
         Status  => HTTP_Codes.OK);
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
      use AWS.Status;

      Queue : Call_Queue.Call_Queue_Type;
      Queue_Length : Ada.Containers.Count_Type;
      JSON : JSON_String;
   begin
      AMI.Action.Action_Manager.Ping;

      Queue := Call_Queue.Get_Queue;
      Queue_Length := Call_Queue.Queue_Length;

      JSON := Call_Queue_JSON.Convert_Queue (Queue, Queue_Length);

      return  Response.Build_JSON_Response
        (Request => Request,
         Content => JSON,
         Status  => HTTP_Codes.OK);
   end Get_Queue;

end Call_Queue_Handler;
