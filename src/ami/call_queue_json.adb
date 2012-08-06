with Ada.Calendar,
     Ada.Calendar.Conversions,
     Ada.Strings.Fixed,
     Ada.Strings.Unbounded;

with Interfaces.C;

package body Call_Queue_JSON is
   use GNATCOLL.JSON;

   Length_String : constant String := "Length";

   function Convert_Call (Call : in Call_Queue.Call_Type)
                          return JSON_String is
      JSON : JSON_Value;
   begin
      JSON := Convert_Call_To_JSON_Object (Call => Call);

      return To_JSON_String (JSON.Write);
   end Convert_Call;

   function Convert_Call_To_JSON_Object (Call : in Call_Queue.Call_Type)
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
      CompanyID : Ada.Strings.Unbounded.Unbounded_String;
      Compnay_prefix : constant String := "org_id";
   begin
      if Call /= Call_Queue.null_Call then
         CompanyID := Ada.Strings.Unbounded.Tail
           (Call.Queue,
            Ada.Strings.Unbounded.Length (Call.Queue) -
            Compnay_prefix'Length);

         Value.Set_Field ("Channel", Call.Channel);
         Value.Set_Field ("CallerIDNum", Call.CallerIDNum);
         Value.Set_Field ("CallerIDName", Call.CallerIDName);
         Value.Set_Field ("CompanyID", CompanyID);
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

   function Convert_Length (Length : in Ada.Containers.Count_Type)
                            return JSON_String is

      Text : constant String :=
        Ada.Strings.Fixed.Trim (Integer (Length)'Img, Ada.Strings.Left);
      JSON : constant JSON_Value := Create_Object;
   begin
      JSON.Set_Field (Length_String, Text);

      return To_JSON_String (JSON.Write);
   end Convert_Length;

   function Convert_Queue (Queue : in Call_Queue.Call_Queue_Type;
                           Queue_Length : in Ada.Containers.Count_Type)
                           return JSON_String is
      use Call_Queue;

      JSON_List : JSON_Array;
      Value     : JSON_Value;

      Result : constant JSON_Value := Create_Object;
   begin
      for Priority in Call_Queue.Priority_Level loop
         JSON_List := Empty_Array;
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
         Text : constant String :=
           Ada.Strings.Fixed.Trim (Integer (Queue_Length)'Img,
                                   Ada.Strings.Left);
      begin
         Result.Set_Field (Length_String, Text);
      end;

      return To_JSON_String (Result.Write);
   end Convert_Queue;

end Call_Queue_JSON;
