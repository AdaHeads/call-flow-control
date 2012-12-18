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

with Model.Call;

package Client_Notification.Call is
   use Model;

   type Pickup_Event is new Client_Notification.Instance
     (Persistant => False) with
      record
         Call : Model.Call.Call_Type;
      end record;
   function To_JSON (O : in Pickup_Event) return JSON_Value;
   function Header_Name (O : in Pickup_Event) return String;
   --  function Pickup (C : in Call.Call_Type) return Pickup_Event;

   type Hangup_Event is new Client_Notification.Instance
     (Persistant => False) with
      record
         Call : Model.Call.Call_Type;
      end record;
   function To_JSON (O : in Hangup_Event) return JSON_Value;
   function Header_Name (O : in Hangup_Event) return String;

   type Park_Event is new Client_Notification.Instance
     (Persistant => False) with
      record
         Call : Model.Call.Call_Type;
      end record;
   function To_JSON (O : in Park_Event) return JSON_Value;
   function Header_Name (O : in Park_Event) return String;

   function Park (C : in Model.Call.Call_Type) return Park_Event;

   type Bridge_Event is new Client_Notification.Instance
     (Persistant => False) with
      record
         Call  : Model.Call.Call_Type;
         Call2 : Model.Call.Call_Type;
      end record;
   function To_JSON (O : in Bridge_Event) return JSON_Value;
   function Header_Name (O : in Bridge_Event) return String;

private
   Pickup_Header : constant String := "call_hangup";
   Hangup_Header : constant String := "call_pickup";
   Park_Header   : constant String := "call_park";
   Bridge_Header : constant String := "call_bridge";
end Client_Notification.Call;
