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

   type Pickup_Event is new Client_Notification.Instance
     (Persistent => False) with
      record
         Call     : Model.Call.Instance;
      end record;
   function To_JSON (O : in Pickup_Event) return JSON_Value;
   function Header_Name (O : in Pickup_Event) return String;
   function Pickup (C : in Model.Call.Instance) return Pickup_Event;

   type Hangup_Event is new Client_Notification.Instance
     (Persistent => False) with
      record
         Call : Model.Call.Instance;
      end record;
   function To_JSON (O : in Hangup_Event) return JSON_Value;
   function Header_Name (O : in Hangup_Event) return String;

   function Hangup (C : in Model.Call.Instance) return Hangup_Event;

   type Park_Event is new Client_Notification.Instance
     (Persistent => False) with
      record
         Call : Model.Call.Instance;
      end record;
   function To_JSON (O : in Park_Event) return JSON_Value;
   function Header_Name (O : in Park_Event) return String;

   function Park (C : in Model.Call.Instance) return Park_Event;

   type Unpark_Event is new Client_Notification.Instance
     (Persistent => False) with
      record
         Call : Model.Call.Instance;
      end record;
   function To_JSON (O : in Unpark_Event) return JSON_Value;
   function Header_Name (O : in Unpark_Event) return String;

   function Unpark (C : in Model.Call.Instance) return Unpark_Event;

   type Bridge_Event is new Client_Notification.Instance
     (Persistent => False) with
      record
         Call  : Model.Call.Instance;
      end record;
   function To_JSON (O : in Bridge_Event) return JSON_Value;
   function Header_Name (O : in Bridge_Event) return String;

   type Originate_Success_Event is new Client_Notification.Instance
     (Persistent => False) with
      record
         Call  : Model.Call.Instance;
      end record;
   function To_JSON (O : in Originate_Success_Event) return JSON_Value;
   function Header_Name (O : in Originate_Success_Event) return String;

   function Originate_Success (C : in Model.Call.Instance)
                               return Originate_Success_Event;

   type Originate_Failed_Event is new Client_Notification.Instance
     (Persistent => False) with
      record
         Call  : Model.Call.Instance;
      end record;
   function To_JSON (O : in Originate_Failed_Event) return JSON_Value;
   function Header_Name (O : in Originate_Failed_Event) return String;

   function Originate_Failed (C : in Model.Call.Instance)
                               return Originate_Failed_Event;

private
   Pickup_Header            : constant String := "call_pickup";
   Hangup_Header            : constant String := "call_hangup";
   Park_Header              : constant String := "call_park";
   Unpark_Header            : constant String := "call_unpark";
   Bridge_Header            : constant String := "call_bridge";
   Originate_Failed_Header  : constant String := "originate_failed";
   Originate_Success_Header : constant String := "originate_success";

end Client_Notification.Call;
