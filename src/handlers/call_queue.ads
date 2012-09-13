with AWS.Status,
     AWS.Response;

package Call_Queue is

   function Call_Answer (Request : in AWS.Status.Data)
                      return AWS.Response.Data;
   --  returns the first call in the list.

   function Call_Hangup (Request : in AWS.Status.Data)
                    return AWS.Response.Data;

--     function Set_Call_On_Hold (Request : in AWS.Status.Data)
--                                return AWS.Response.Data;

   function Call_Park (Request : in AWS.Status.Data)
                       return AWS.Response.Data;
   --  Places the current call, in the parked list.

--     function Call_Unpark (Request : in AWS.Status.Data)
--                           return AWS.Response.Data;
--     Obsolescent.

   function Get_Queue (Request : in AWS.Status.Data)
                       return AWS.Response.Data;
   --  returns the entire Call Queue, in JSON format.

   function Length (Request : in AWS.Status.Data)
                        return AWS.Response.Data;
   --  returns the number of calls waiting in the callqueue.

end Call_Queue;
