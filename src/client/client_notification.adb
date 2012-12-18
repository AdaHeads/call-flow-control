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

with Ada.Characters.Handling;

with Common;

package body Client_Notification is
   use Ada.Characters.Handling;

   procedure JSON_Append (Node  : in JSON_Value;
                          Key   : in String;
                          Value : in JSON_Value) is
   begin
      Node.Get (Field => "notification").Set_Field (Key, Value);
   end JSON_Append;

   function JSON_Root (O : in Instance'Class) return JSON_Value is
      Root_JSON         : constant JSON_Value := Create_Object;
      Notification_JSON : constant JSON_Value := Create_Object;
   begin
      Notification_JSON.Set_Field ("persistent", To_Lower (O.Persistant'Img));
      Notification_JSON.Set_Field ("event", O.Header_Name);

      Root_JSON.Set_Field ("timestamp", Common.Unix_Timestamp
                           (Common.Current_Time));
      Root_JSON.Set_Field ("notification", Notification_JSON);

      return Root_JSON;
   end JSON_Root;

end Client_Notification;
