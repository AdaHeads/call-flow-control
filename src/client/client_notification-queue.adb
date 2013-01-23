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

package body Client_Notification.Queue is

   -------------------
   --  Header_Name  --
   -------------------

   function Header_Name (O : in Join_Event) return String is
      pragma Unreferenced (O);
   begin
      return Join_Header;
   end Header_Name;

   -------------------
   --  Header_Name  --
   -------------------

   function Header_Name (O : in Leave_Event) return String is
      pragma Unreferenced (O);
   begin
      return Leave_Header;
   end Header_Name;

   ------------
   --  Join  --
   ------------

   function Join (C : in Call.Instance) return Join_Event is
   begin
      return (Instance with Persistent => False, Joined_Call => C);
   end Join;

   -------------
   --  Leave  --
   -------------

   function Leave (C : in Call.Instance) return Leave_Event is
   begin
      return (Instance with Persistent => False, Left_Call => C);
   end Leave;

   ---------------
   --  To_JSON  --
   ---------------

   function To_JSON (O : in Join_Event) return JSON_Value is
      Notification_JSON : constant JSON_Value := O.JSON_Root;
   begin
      JSON_Append (Notification_JSON, "call", O.Joined_Call.To_JSON);

      return Notification_JSON;
   end To_JSON;

   ---------------
   --  To_JSON  --
   ---------------

   function To_JSON (O : in Leave_Event) return JSON_Value is
      Notification_JSON : constant JSON_Value := O.JSON_Root;
   begin
      JSON_Append (Notification_JSON, "call", O.Left_Call.To_JSON);

      return Notification_JSON;
   end To_JSON;

end Client_Notification.Queue;
