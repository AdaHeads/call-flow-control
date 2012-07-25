with Call_Queue;
with GNATCOLL.JSON;
with Ada.Containers;
with Common; use Common;
--  TODO All the functions returns AWS.Response.Data,
--       but this is a JSON package, make it return JSON Strings.
--       and the same problem with the parameter. I don't want to know about it
package Call_Queue_JSON is

   function Convert_Queue (Queue : in Call_Queue.Call_Queue_Type;
                          Queue_Length : in Ada.Containers.Count_Type)
                           return JSON_String;
   --  returns the entire Call Queue, in JSON format.

   function Convert_Length (Length : in Ada.Containers.Count_Type)
                            return JSON_String;
   --  returns the number of calls waiting in the calling queue.

   function Convert_Call (Call : in Call_Queue.Call_Type)
                          return JSON_String;
   --  returns the first call in the list.
private
   function Convert_Call_To_JSON_Object (Call : Call_Queue.Call_Type)
                                         return GNATCOLL.JSON.JSON_Value;
   --  takes a call and converts it to a JSON object.
end Call_Queue_JSON;
