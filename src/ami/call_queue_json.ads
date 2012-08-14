with Ada.Containers;
with Call_Queue,
     Common;
private with GNATCOLL.JSON;

--  This package can return callqueue information and it in JSON format.
package Call_Queue_JSON is
   use Common;

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

   function Status_Message (Title   : in String;
                            Message : in String) return JSON_String;
private
   function Convert_Call_To_JSON_Object (Call : in Call_Queue.Call_Type)
                                         return GNATCOLL.JSON.JSON_Value;
   --  takes a call and converts it to a JSON object.
end Call_Queue_JSON;
