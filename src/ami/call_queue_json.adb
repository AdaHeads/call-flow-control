with Ada.Calendar,
     Ada.Calendar.Conversions,
     Ada.Strings.Fixed,
     Ada.Strings.Unbounded;

with Interfaces.C;

package body Call_Queue_JSON is
   use GNATCOLL.JSON;

   Length_String : constant String := "Length";

   function Convert_Call (Call : in Call_List.Call_Type)
                          return JSON_String is
      JSON : JSON_Value;
   begin
      JSON := Convert_Call_To_JSON_Object (Call => Call);

      return To_JSON_String (JSON.Write);
   end Convert_Call;

   function Convert_Call_To_JSON_Object (Call : in Call_List.Call_Type)
                                         return GNATCOLL.JSON.JSON_Value is
      use Ada.Calendar;
      use Ada.Calendar.Conversions;
      use Call_List;

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
      if Call /= Call_List.null_Call then
         CompanyID := Ada.Strings.Unbounded.Tail
           (Call.Queue,
            Ada.Strings.Unbounded.Length (Call.Queue) -
            Compnay_prefix'Length);

         Value.Set_Field ("channel", Call.Channel);
         Value.Set_Field ("caller_id", Call.CallerIDNum);
         Value.Set_Field ("org_id", CompanyID);
         Value.Set_Field ("uniqueid", Call.Uniqueid);
         Value.Set_Field ("arrival_time", Unix_Timestamp (Call.Arrived));

         if Call.Is_Picked_Up then
            Value.Set_Field ("pickup_time", Unix_Timestamp (Call.Picked_Up));
         end if;

         if Call.Is_Ended then
            Value.Set_Field ("ended", Unix_Timestamp (Call.Ended));
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

   function Convert_Queue (Queue : in Call_List.Call_List_Type.Vector)
                           return JSON_String is
      use Call_List;

      JSON_List : JSON_Array;
      Value     : JSON_Value;

      Result : constant JSON_Value := Create_Object;
      Cursor : Call_List_Type.Cursor;
   begin
      JSON_List := Empty_Array;

      Cursor := Queue.First;
      loop
         exit when not Call_List_Type.Has_Element (Cursor);
         Value := Convert_Call_To_JSON_Object
           (Call_List_Type.Element (Cursor));
         Append (JSON_List, Value);
      end loop;
         --------------------------------------------------------------
         Result.Set_Field ("calls",
                           JSON_List);

      return To_JSON_String (Result.Write);
   end Convert_Queue;

   function Status_Message (Title   : in String;
                            Message : in String) return JSON_String is
      JSON : constant JSON_Value := Create_Object;
   begin
      JSON.Set_Field ("status", Title);
      JSON.Set_Field ("description", Message);
      return To_JSON_String (JSON.Write);
   end Status_Message;

end Call_Queue_JSON;
