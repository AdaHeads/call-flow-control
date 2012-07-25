with Call_Queue;
with AWS.Response;
with AWS.Status;
with GNATCOLL.JSON;
--  TODO All the functions returns AWS.Response.Data,
--       but this is a JSON package, make it return JSON Strings.
--       and the same problem with the parameter. I don't want to know about it
package Call_Queue_JSON is

   function Get_Queue (Request : in AWS.Status.Data)
                       return AWS.Response.Data;
   --  returns the entire Call Queue, in JSON format.

   function Get_Length (Request : in AWS.Status.Data)
                        return AWS.Response.Data;
   --  returns the number of calls waiting in the calling queue.

   function Get_Call (Request : in AWS.Status.Data)
                      return AWS.Response.Data;
   --  returns the first call in the list.
private
   function Convert_Call_To_JSON_Object (Call : Call_Queue.Call_Type)
                                         return GNATCOLL.JSON.JSON_Value;
   --  takes a call and converts it to a JSON object.
end Call_Queue_JSON;
