with AWS.Status,
     AWS.Response;

package Call_Queue_Handler is
   function Get_Queue (Request : in AWS.Status.Data)
                       return AWS.Response.Data;
   --  returns the entire Call Queue, in JSON format.

   function Get_Length (Request : in AWS.Status.Data)
                        return AWS.Response.Data;
   --  returns the number of calls waiting in the callqueue.

   function Get_Call (Request : in AWS.Status.Data)
                      return AWS.Response.Data;
   --  returns the first call in the list.

   function Hangup (Request : in AWS.Status.Data)
                    return AWS.Response.Data;

--     function Set_Call_On_Hold (Request : in AWS.Status.Data)
--                                return AWS.Response.Data;

   function Park_Call (Request : in AWS.Status.Data)
                       return AWS.Response.Data;
   --  Places the current call, in the parked list.

   function Unpark_Call (Request : in AWS.Status.Data)
                          return AWS.Response.Data;
end Call_Queue_Handler;
