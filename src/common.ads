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

package Common is

   function Current_Time return Ada.Calendar.Time renames Ada.Calendar.Clock;
   subtype Time is Ada.Calendar.Time;

   function U
     (S : in String)
      return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;

   type JSON_String is new Ada.Strings.Unbounded.Unbounded_String;

   Null_JSON_String : constant JSON_String :=
                        JSON_String
                          (Ada.Strings.Unbounded.Null_Unbounded_String);

   function To_JSON_String
     (Source : in String)
      return JSON_String
      renames To_Unbounded_String;

   function Unix_Timestamp
     (Date : in Time)
     return String;
   --  Convert and trim an Ada.Calendar.Time type to a Unix timestamp
   --  String.

end Common;
