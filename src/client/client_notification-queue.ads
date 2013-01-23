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

with PBX.Call;

package Client_Notification.Queue is
   use PBX;

   type Join_Event is new Client_Notification.Instance
     (Persistent => False) with
      record
         Joined_Call : Call.Instance;
      end record;
   function To_JSON (O : in Join_Event) return JSON_Value;
   function Header_Name (O : in Join_Event) return String;

   type Leave_Event is new Client_Notification.Instance
     (Persistent => False) with
      record
         Left_Call : Call.Instance;
      end record;
   function To_JSON (O : in Leave_Event) return JSON_Value;
   function Header_Name (O : in Leave_Event) return String;

   function Join (C : in Call.Instance) return Join_Event;

   function Leave (C : in Call.Instance) return Leave_Event;

private

   Join_Header  : constant String := "queue_join";
   Leave_Header : constant String := "queue_leave";

end Client_Notification.Queue;
