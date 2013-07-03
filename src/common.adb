-------------------------------------------------------------------------------
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

with Ada.Calendar.Conversions;
with Ada.Strings.Fixed;
with Ada.Strings;

package body Common is

   -----------------------------
   --  String_To_JSON_Object  --
   -----------------------------

   function String_To_JSON_Object
     (Value : in String)
      return GNATCOLL.JSON.JSON_Value
   is
      use GNATCOLL.JSON;
   begin
      if Value = "null" or Value = "" then
         return JSON_Null;
      else
         return Read (Value, "String_To_JSON_Object error");
      end if;
   end String_To_JSON_Object;

   -----------------------------
   --  String_To_JSON_Object  --
   -----------------------------

   function String_To_JSON_Object
     (Value : in JSON_String)
      return GNATCOLL.JSON.JSON_Value
   is
   begin
      return String_To_JSON_Object (To_String (Value));
   end String_To_JSON_Object;

   ----------------------
   --  Unix_Timestamp  --
   ----------------------

   function Unix_Timestamp
     (Date : in Time)
      return String
   is
      use Ada.Strings;
   begin
      return Fixed.Trim
        (Source => Long_Unsigned'Image (Unix_Timestamp (Date)),
         Side   => Left);
   end Unix_Timestamp;

   function Unix_Timestamp
     (Date : in Time)
      return Long_Unsigned
   is
   begin
      return Long_Unsigned (Ada.Calendar.Conversions.To_Unix_Time (Date));
   end Unix_Timestamp;

end Common;
