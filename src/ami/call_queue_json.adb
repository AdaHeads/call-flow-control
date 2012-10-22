-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                              Call_Queue_JSON                              --
--                                                                           --
--                                  BODY                                     --
--                                                                           --
--                     Copyright (C) 2012-, AdaHeads K/S                     --
--                                                                           --
--  This is free software;  you can redistribute it and/or modify it         --
--  under terms of the  GNU General Public License  as published by the      --
--  Free Software  Foundation;  either version 3,  or (at your  option) any  --
--  later version. This library is distributed in the hope that it will be   --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     --
--  You should have received a copy of the GNU General Public License and    --
--  a copy of the GCC Runtime Library Exception along with this program;     --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
--  <http://www.gnu.org/licenses/>.                                          --
--                                                                           --
-------------------------------------------------------------------------------

with Ada.Calendar;
with Ada.Calendar.Conversions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Errors;
with Interfaces.C;
with Yolk.Log;

package body Call_Queue_JSON is
   use GNATCOLL.JSON;
   use Yolk.Log;

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
      Trace (Debug, "CALL_QUEUE_JSON DEBUG - " &
                        Ada.Strings.Unbounded.To_String (Call.Queue));
      if Call /= Call_List.Null_Call then
         Trace (Debug, "CALL_QUEUE_JSON DEBUG: " & Call.State'Img);
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
   exception
      when Event : others =>
         pragma Unreferenced (Event);
         --  TODO: Switch to the System_Message method of handling error.
--           Log_Exception (Err, "Queue: [" &
--                              Ada.Strings.Unbounded.To_String (Call.Queue) &
--                                "]");
         null;
         raise;
   end Convert_Call_To_JSON_Object;

   function Convert_Length (Length : in Ada.Containers.Count_Type)
                            return JSON_String is

      Text : constant String :=
               Ada.Strings.Fixed.Trim (Integer (Length)'Img, Ada.Strings.Left);
      --  ???? Count_Type is ultimately just an integer. Why this conversion to
      --  String?
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
   begin
      JSON_List := Empty_Array;

      for item of Queue loop
         Value := Convert_Call_To_JSON_Object (item);
         Append (JSON_List, Value);
      end loop;

      Result.Set_Field ("calls", JSON_List);

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
