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
with Common;

package body PBX.Event.Join is
   use Common;

   function Create (C : in Call.Call_Type) return Instance is
   begin
      return (Call => C);
   end Create;

   function To_JSON (O : in Instance) return JSON_Value is
      JSON              : constant JSON_Value := Create_Object;
      Notification_JSON : constant JSON_Value := Create_Object;
   begin
      Notification_JSON.Set_Field ("call", O.Call.To_JSON);
      Notification_JSON.Set_Field ("persistent", False);
      Notification_JSON.Set_Field ("event", "queue_join");

      JSON.Set_Field ("timestamp", Unix_Timestamp (Current_Time));
      JSON.Set_Field ("notification", Notification_JSON);

      return JSON;
   end To_JSON;

end PBX.Event.Join;
