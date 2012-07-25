with Ada.Calendar;
with Ada.Calendar.Conversions;
with Interfaces.C;
with Ada.Strings.Fixed;
with Response;
with HTTP_Codes;
with Ada.Containers;
with Common;
with Socket;
package body Call_Queue_JSON is
   use GNATCOLL.JSON;
   use Common;
   --     function Convert_Call (Call : Call_Queue.Call_Type)
   --                            return JSON_String is
   --        use Call_Queue;
   --        use GNATCOLL.JSON;
   --        Value : constant JSON_Value := Convert_Call_To_JSON_Object (Call);
   --     begin
   --        return To_JSON_String (Value.Write);
   --     end Convert_Call;

   Length_String : constant String := "Length";

   function Convert_Call_To_JSON_Object (Call : Call_Queue.Call_Type)
                                         return GNATCOLL.JSON.JSON_Value is
      use Ada.Calendar;
      use Ada.Calendar.Conversions;
      use Call_Queue;
      function Unix_Timestamp
        (Date : in Time)
         return String;
      --  Convert and trim an Ada.Calendar.Time type to a Unix timestamp
      --  String.

      function Unix_Timestamp
        (Date : in Time)
         return String
      is
         use Ada.Strings;
         use Interfaces.C;
      begin
         return Fixed.Trim
           (Source => long'Image (To_Unix_Time (Date)),
            Side   => Left);
      end Unix_Timestamp;

      Value : constant JSON_Value := Create_Object;
   begin
      if Call /= Call_Queue.null_Call then
         Value.Set_Field ("Channel", Call.Channel);
         Value.Set_Field ("CallerIDNum", Call.CallerIDNum);
         Value.Set_Field ("CallerIDName", Call.CallerIDName);
         Value.Set_Field ("CompanyName", Call.Queue);
         Value.Set_Field ("Position", Call.Position);
         Value.Set_Field ("Count", Call.Count);
         Value.Set_Field ("Uniqueid", Call.Uniqueid);
         Value.Set_Field ("Arrived", Unix_Timestamp (Call.Arrived));

         if Call.Is_Picked_Up then
            Value.Set_Field ("Picked_Up", Unix_Timestamp (Call.Picked_Up));
         end if;
         if Call.Is_Ended then
            Value.Set_Field ("Ended", Unix_Timestamp (Call.Ended));
         end if;
      end if;
      return Value;
   end Convert_Call_To_JSON_Object;

   function Get_Call (Request : in AWS.Status.Data)
                      return AWS.Response.Data is
--        use AWS.Parameters;
      use AWS.Status;
      Call : Call_Queue.Call_Type;
      Agent : constant String := Parameters (Request).Get ("agent");
      Unitqueid : constant String :=  Parameters (Request).Get ("uniqueid");

      JSON : JSON_Value;
   begin
      Socket.Get_Call (Uniqueid => Unitqueid,
                       Agent    => Agent,
                       Call     => Call);

      JSON := Convert_Call_To_JSON_Object (Call => Call);

      return Response.Build_JSON_Response
        (Request => Request,
         Content => To_JSON_String (JSON.Write),
         Status  => HTTP_Codes.OK);
   end Get_Call;

   function Get_Length (Request : in AWS.Status.Data)
                        return AWS.Response.Data is
      Queue_Length : constant Ada.Containers.Count_Type :=
        Call_Queue.Queue_Length;
      Text : constant String :=
        Ada.Strings.Fixed.Trim (Integer (Queue_Length)'Img, Ada.Strings.Left);
      JSON : constant JSON_Value := Create_Object;
   begin
      JSON.Set_Field (Length_String, Text);

      return Response.Build_JSON_Response
        (Request => Request,
         Content => To_JSON_String (JSON.Write),
         Status  => HTTP_Codes.OK);
   end Get_Length;

   function Get_Queue (Request : in AWS.Status.Data)
                            return AWS.Response.Data is
      use Call_Queue;

      Queue : constant Call_Queue_Type := Call_Queue.Get_Queue;
      JSON_List : JSON_Array := Empty_Array;
      Value : JSON_Value;

      Result : constant JSON_Value := Create_Object;
   begin
      for Priority in Call_Queue.Priority_Level loop
         for Index in Queue (Priority).First_Index ..
           Queue (Priority).Last_Index loop

            Value := Convert_Call_To_JSON_Object
              (Queue (Priority).Element (Index));
            Append (JSON_List, Value);

         end loop;
         Result.Set_Field (Priority'Img, JSON_List);
      end loop;
      --  TODO Find another methode for finding the length,
      --        because no we are asking on the length of an other Queue.
      declare
         Queue_Length : constant Ada.Containers.Count_Type :=
           Call_Queue.Queue_Length;
         Text : constant String :=
           Ada.Strings.Fixed.Trim (Integer (Queue_Length)'Img,
                                   Ada.Strings.Left);
      begin
         Result.Set_Field (Length_String, Text);
      end;

      return Response.Build_JSON_Response
        (Request => Request,
         Content => To_JSON_String (Result.Write),
         Status  => HTTP_Codes.OK);
   end Get_Queue;

   --     function Convert_Call_Length (Item : Ada.Containers.Count_Type)
   --                                   return JSON_String is
   --        use GNATCOLL.JSON;
   --        Length : constant String :=
   --          Ada.Strings.Fixed.Trim (Integer (Item)'Img, Ada.Strings.Left);
   --        JSON : constant JSON_Value := Create_Object;
   --     begin
   --        JSON.Set_Field ("Length", Length);
   --        return To_JSON_String (JSON.Write);
   --     end Convert_Call_Length;

end Call_Queue_JSON;
