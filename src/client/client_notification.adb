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

   function Create_With_Call (Name : in Event_Names;
                              Call : in Model.Call.Instance)
                              return Instance;
   --  Convenience method for constructing the various types of event objects
   --  regarding calls.

   -----------------
   --  Call_Lock  --
   -----------------

   function Call_Lock (C : in Model.Call.Instance) return Instance is
   begin
      return Create_With_Call (Name => Call_Lock,
                               Call => C);
   end Call_Lock;

   ------------------
   --  Call_Offer  --
   ------------------

   function Call_Offer (C : in Model.Call.Instance) return Instance is
   begin
      return Create_With_Call (Name => Call_Offer,
                               Call => C);
   end Call_Offer;

   ------------------
   --  Call_State  --
   ------------------

   function Call_State (C : in Model.Call.Instance) return Instance is
   begin
      return Create_With_Call (Name => Call_State,
                               Call => C);
   end Call_State;

   ---------------------
   --  Call_Transfer  --
   ---------------------

   function Call_Transfer (C : in Model.Call.Instance) return Instance is
   begin
      return Create_With_Call (Name => Call_Transfer,
                               Call => C);
   end Call_Transfer;

   -------------------
   --  Call_Unlock  --
   -------------------

   function Call_Unlock (C : in Model.Call.Instance) return Instance is
   begin
      return Create_With_Call (Name => Call_Unlock,
                               Call => C);
   end Call_Unlock;

   ------------------------
   --  Create_With_Call  --
   ------------------------

   function Create_With_Call (Name : in Event_Names;
                              Call : in Model.Call.Instance)
                              return Instance is
   begin
      return Notification : Instance (Name => Name) do
         Notification.Root := Notification.JSON_Root;
         JSON_Append (Notification.Root, "call", Call.To_JSON);
      end return;
   end Create_With_Call;

   --------------
   --  Hangup  --
   --------------

   function Hangup (C : in Model.Call.Instance) return Instance is
   begin
      return Create_With_Call (Name => Call_Hangup,
                               Call => C);
   end Hangup;

   -------------
   --  Image  --
   -------------

   function Image (Event : in Instance'Class) return String is
   begin
      return Image (Event.Name);
   end Image;

   -------------
   --  Image  --
   -------------

   function Image (Item : in Event_Names) return String is
   begin
      return To_Lower (Event_Names'Image (Item));
   end Image;

   ------------
   --  Join  --
   ------------

   function Join (C : in Model.Call.Instance) return Instance is
   begin
      return Create_With_Call (Name => Queue_Join,
                                 Call => C);
   end Join;

   -------------------
   --  JSON_Append  --
   -------------------

   procedure JSON_Append (Node  : in JSON_Value;
                          Key   : in String;
                          Value : in JSON_Value) is
   begin
      Node.Get (Field => Root_Field).Set_Field (Key, Value);
   end JSON_Append;

   -----------------
   --  JSON_Root  --
   -----------------

   function JSON_Root (Event : in Instance'Class) return JSON_Value is
      Root_JSON         : constant JSON_Value := Create_Object;
      Notification_JSON : constant JSON_Value := Create_Object;
   begin
      Notification_JSON.Set_Field (Event_Field, Image (Event.Name));

      Root_JSON.Set_Field (Timestamp_Field, Common.Unix_Timestamp
                           (Common.Current_Time));
      Root_JSON.Set_Field (Root_Field, Notification_JSON);

      return Root_JSON;
   end JSON_Root;

   -------------
   --  Leave  --
   -------------

   function Leave (C : in Model.Call.Instance) return Instance is
   begin
      return Create_With_Call (Name => Queue_Leave,
                               Call => C);
   end Leave;

   ------------
   --  Park  --
   ------------

   function Park (C : in Model.Call.Instance) return Instance is
   begin
      return Create_With_Call (Name => Call_Park,
                               Call => C);
   end Park;

   ------------------
   --  Peer_State  --
   ------------------

   function Peer_State (P : in Model.Peer.Instance) return Instance is
   begin
      return Notification : Instance (Name => Peer_State) do
         Notification.Root := Notification.JSON_Root;
         JSON_Append (Notification.Root, "peer", P.To_JSON);
      end return;
   end Peer_State;

   --------------
   --  Pickup  --
   --------------

   function Pickup (C : in Model.Call.Instance) return Instance is
   begin
      return Create_With_Call (Name => Call_Pickup,
                               Call => C);
   end Pickup;

   ---------------
   --  To_JSON  --
   ---------------

   function To_JSON (Event : in Instance'Class) return JSON_Value is
   begin
      return Event.Root;
   end To_JSON;

   --------------
   --  Unpark  --
   --------------

   function Unpark (C : in Model.Call.Instance) return Instance is
   begin
      return Create_With_Call (Name => Call_Unpark,
                               Call => C);
   end Unpark;

end Client_Notification;
