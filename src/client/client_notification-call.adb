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
package body Client_Notification.Call is

   function Hangup (C : in PBX.Call.Instance) return Hangup_Event is
   begin
      return (Instance with Persistant => False, Call => C);
   end Hangup;

   -----------------------------
   --  Header_Name functions  --
   -----------------------------

   function Header_Name (O : in Pickup_Event) return String is
      pragma Unreferenced (O);
   begin
      return Pickup_Header;
   end Header_Name;

   function Header_Name (O : in Hangup_Event) return String is
      pragma Unreferenced (O);
   begin
      return Hangup_Header;
   end Header_Name;

   function Header_Name (O : in Park_Event) return String is
      pragma Unreferenced (O);
   begin
      return Park_Header;
   end Header_Name;

   function Header_Name (O : in Bridge_Event) return String is
      pragma Unreferenced (O);
   begin
      return Bridge_Header;
   end Header_Name;

   function Park (C : in PBX.Call.Instance) return Park_Event is
   begin
      return (Instance with Persistant => False, Call => C);
   end Park;

   function Pickup (C : in PBX.Call.Instance) return Pickup_Event
   is
   begin
      return (Instance with Persistant => False, Call => C);
   end Pickup;

   --------------------------
   --  To_JSON for Hangup  --
   --------------------------

   function To_JSON (O : in Hangup_Event) return JSON_Value is
      Notification_JSON : constant JSON_Value := O.JSON_Root;
   begin
      JSON_Append (Notification_JSON, "call", O.Call.To_JSON);

      return Notification_JSON;

   end To_JSON;

   ------------------------
   --  To_JSON for Park  --
   ------------------------

   function To_JSON (O : in Park_Event) return JSON_Value is
      Notification_JSON : constant JSON_Value := O.JSON_Root;
   begin
      JSON_Append (Notification_JSON, "call", O.Call.To_JSON);

      return Notification_JSON;

   end To_JSON;

   --------------------------
   --  To_JSON for Pickup  --
   --------------------------

   function To_JSON (O : in Pickup_Event) return JSON_Value is
      Notification_JSON : constant JSON_Value := O.JSON_Root;
   begin
      JSON_Append (Notification_JSON, "call", O.Call.To_JSON);
      return Notification_JSON;

   end To_JSON;

   --------------------------
   --  To_JSON for Bridge  --
   --------------------------

   function To_JSON (O : in Bridge_Event) return JSON_Value is
      Notification_JSON : constant JSON_Value := O.JSON_Root;
   begin
      JSON_Append (Notification_JSON, "call", O.Call.To_JSON);
      JSON_Append (Notification_JSON, "destination", O.Call.To_JSON);

      return Notification_JSON;
   end To_JSON;

end Client_Notification.Call;
