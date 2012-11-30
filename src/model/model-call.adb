-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                Model.Call                                 --
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

package body Model.Call is

   -- -------------------- --
   -- Overloaded operators --
   -- -------------------- --

   function "=" (Left  : in Call_Type;
                 Right : in Call_Type) return Boolean is
   begin
      return (Left.ID.Timestamp = Right.ID.Timestamp) and
                (Left.ID.Sequence = Right.ID.Sequence);
   end  "=";

   -- -------------------- --
   -- Conversion Utilities --
   -- -------------------- --

   function To_JSON (Call : in Call_Type) return JSON_Value is
      Root : constant JSON_Value := Create_Object;
      Value : constant JSON_Value := Create_Object;
      Org_ID : Ada.Strings.Unbounded.Unbounded_String;
      Org_Prefix : constant String := "org_id";
   begin
      if Call /= Null_Call then
         Org_ID := Ada.Strings.Unbounded.Tail
           (Call.Queue,
            Ada.Strings.Unbounded.Length (Call.Queue) - Org_Prefix'Length);

         Value.Set_Field ("channel", Call.Channel_ID.To_String);
         --  Value.Set_Field ("caller_id", Call.);
         Value.Set_Field ("org_id", Org_ID);
         Value.Set_Field ("call_id",  To_String (Call.ID));
         Value.Set_Field ("arrival_time", Unix_Timestamp (Call.Arrived));
         Root.Set_Field ("call", Value);
      end if;
      return Root;
   end To_JSON;

   --  Returns a call in String format.
   function To_String (Call : in Call_Type) return String is
      Response : Unbounded_String;
   begin
      Append (Response, "ID => " & Call.ID.To_String);
      Append (Response, ", Channel => " & Call.Channel_ID.To_String);
      Append (Response, ", Queue => "    & To_String (Call.Queue));
      Append (Response, ", State => "    & Call.State'Img);
      return To_String (Response);
   end To_String;

end Model.Call;
