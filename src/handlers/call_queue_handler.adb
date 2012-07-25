with Ada.Containers;
with Call_Queue;
with Call_Queue_JSON;
with Common; use Common;
with Response;
with HTTP_Codes;
with Socket;

package body Call_Queue_Handler is
   --  returns the first call in the list.
   function Get_Call (Request : in AWS.Status.Data)
                      return AWS.Response.Data is
      use AWS.Status;

      Agent : constant String := Parameters (Request).Get ("agent");
      Unitqueid : constant String :=  Parameters (Request).Get ("uniqueid");
      Call : Call_Queue.Call_Type;
      JSON : JSON_String;
   begin
      Socket.Get_Call (Uniqueid => Unitqueid,
                       Agent    => Agent,
                       Call     => Call);

      JSON := Call_Queue_JSON.Convert_Call (Call);

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
      Queue := Call_Queue.Get_Queue;
      Queue_Length := Call_Queue.Queue_Length;

      JSON := Call_Queue_JSON.Convert_Queue (Queue, Queue_Length);

      return  Response.Build_JSON_Response
             (Request => Request,
              Content => JSON,
              Status  => HTTP_Codes.OK);
   end Get_Queue;

end Call_Queue_Handler;
