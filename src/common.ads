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

with Ada.Calendar;
with Ada.Strings.Unbounded;

with GNATCOLL.JSON;

package Common is

   type Long_Unsigned is mod 2**64;

   subtype Time is Ada.Calendar.Time;

   Null_Time : constant Time := Ada.Calendar.Time_Of
     (Year    => 1901,
      Month   => 1,
      Day     => 1,
      Seconds => 0.0);

   function Current_Time
     return Ada.Calendar.Time
     renames Ada.Calendar.Clock;

   function Unix_Timestamp
     (Date : in Time)
      return String;
   --  Convert and trim an Ada.Calendar.Time type to a Unix timestamp
   --  String.

   function Unix_Timestamp (Date : in Time)
       return Long_Unsigned;
   --  Convert an Ada.Calendar.Time type to a 64-bit unsigned.

   type JSON_String is new Ada.Strings.Unbounded.Unbounded_String;

   Null_JSON_String : constant JSON_String :=
                        JSON_String
                          (Ada.Strings.Unbounded.To_Unbounded_String ("{}"));

   function String_To_JSON_Object
     (Value : in String)
      return GNATCOLL.JSON.JSON_Value;
   --  Turn the Value string into a JSON object. In the case of an
   --  empty string or a "null" string a JSON.JSON_Null is returned.

   function String_To_JSON_Object
     (Value : in JSON_String)
      return GNATCOLL.JSON.JSON_Value;
   --  Turn the Value string into a JSON object. In the case of an
   --  empty string or a "null" string a JSON.JSON_Null is returned.

   function To_JSON_String
     (Source : in GNATCOLL.JSON.JSON_Value)
      return JSON_String;

   function U
     (S : in String)
      return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;

end Common;
