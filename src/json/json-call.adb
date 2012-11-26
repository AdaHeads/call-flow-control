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

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with System_Messages;
with Model.Call_ID;
package body JSON.Call is
   use GNATCOLL.JSON;
   use System_Messages;
   use Model.Call_ID;

   Length_String : constant String := "length";

   function Status_Message (Title   : in String;
                            Message : in String) return JSON_String is
      JSON : constant JSON_Value := Create_Object;
   begin
      JSON.Set_Field ("status", Title);
      JSON.Set_Field ("description", Message);
      return To_JSON_String (JSON.Write);
   end Status_Message;

   function To_JSON_Object (Call : in Model.Call.Call_Type)
                           return GNATCOLL.JSON.JSON_Value is
      use Model.Call;

      Root : constant JSON_Value := Create_Object;
      Value : constant JSON_Value := Create_Object;
      Org_ID : Ada.Strings.Unbounded.Unbounded_String;
      Org_Prefix : constant String := "org_id";
   begin
      if Call /= Null_Call then
         Org_ID := Ada.Strings.Unbounded.Tail
           (Call.Queue,
            Ada.Strings.Unbounded.Length (Call.Queue) - Org_Prefix'Length);

         Value.Set_Field ("channel", Call.Channel);
         Value.Set_Field ("caller_id", Call.CallerIDNum);
         Value.Set_Field ("org_id", Org_ID);
         Value.Set_Field ("call_id",  To_String (Call.ID));
         Value.Set_Field ("arrival_time", Unix_Timestamp (Call.Arrived));
         Root.Set_Field ("call", Value);

      end if;
      return Root;
   exception
      when others =>
         System_Messages.Notify
           (Error, "Queue: [" &
              Ada.Strings.Unbounded.To_String (Call.Queue) &
              "]");
      raise;
   end To_JSON_Object;

   function To_JSON_String (Length : in Natural)
                            return JSON_String is

      Text : constant String :=
               Ada.Strings.Fixed.Trim (Integer (Length)'Img, Ada.Strings.Left);
      --  ???? Count_Type is ultimately just an integer. Why this conversion to
      --  String?
      JSON : constant JSON_Value := Create_Object;
   begin
      JSON.Set_Field (Length_String, Text);

      return To_JSON_String (JSON.Write);
   end To_JSON_String;

   function To_JSON_String (Queue : in Model.Calls.Call_List_Type.Map)
                           return JSON_String is
      use Model.Call;

      JSON_List : JSON_Array;
      Value     : JSON_Value;

      Result : constant JSON_Value := Create_Object;
   begin
      JSON_List := Empty_Array;

      for item of Queue loop
         Value := To_JSON_Object (item);
         Append (JSON_List, Value);
      end loop;

      Result.Set_Field ("calls", JSON_List);

      return To_JSON_String (Result.Write);
   end To_JSON_String;

   function To_JSON_String (Call : in Model.Call.Call_Type)
                                return JSON_String is
      JSON : JSON_Value;
   begin
      JSON := To_JSON_Object (Call => Call);

      return To_JSON_String (JSON.Write);
   end To_JSON_String;

end JSON.Call;
