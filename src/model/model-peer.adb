-------------------------------------------------------------------------------
--                                                                           --
--                     Copyright (C) 2013-, AdaHeads K/S                     --
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
with Client_Notification;
with Handlers.Notifications;

package body Model.Peer is
   use Ada.Calendar;

   package Notification renames Handlers.Notifications;

   procedure Bump_Expiry (Object     :    out Instance;
                          Time_Delta : in     Natural) is
   begin
      Object.Expiry_Time := Current_Time + Duration (Time_Delta);
   end Bump_Expiry;

   --------------
   --  Create  --
   --------------

   function Create (Peer_ID : Identification;
                    Values  : JSON_Value) return Instance is
      Is_Registered : Boolean := False;
   begin
      if Values.Get (Field => Contact_String) (1 .. 5) /= "error" then
         Is_Registered := True;
      end if;

      return (Peer_ID     => Peer_ID,
              Values      => Values,
              Registered  => Is_Registered,
              Expiry_Time => <>);
   end Create;

   --------------------------
   --  Get_Identification  --
   --------------------------

   function Get_Identification (Object : in Instance) return String is
   begin
      if not Object.Registered then
         raise Peer_Not_Registered;
      end if;

      return Image (Object.Peer_ID);
   end Get_Identification;

   -------------
   --  Image  --
   -------------

   function Image (Object : in Instance) return String is
   begin
      return Image (Object.Peer_ID) & " " &
      (if Object.Registered then
       "(registered)"
       else
       "(not registered)");
   end Image;

   -------------
   --  Image  --
   -------------

   function Image (Item : in Identification) return String is
   begin
      return To_String (Item);
   end Image;

   ----------------
   --  Register  --
   ----------------

   procedure Register (Object : out Instance) is
   begin
      if not Object.Registered then
         Object.Registered := True;
         Notification.Broadcast
           (Client_Notification.Peer_State (P => Object).To_JSON);
      end if;

   end Register;

   ------------------
   --  Registered  --
   ------------------

   function Registered (Object : in Instance) return Boolean is
   begin
      return Object.Registered;
   end Registered;

   ---------------
   --  To_JSON  --
   ---------------

   function To_JSON (Object : in Instance) return JSON_Value is
      Root       : constant JSON_Value := Create_Object;
      Expires_In : constant Duration :=
        Object.Expiry_Time - Common.Current_Time;
   begin
      if Expires_In < 0.0 then
         Root.Set_Field (Field_Name => Expires_String,
                         Field      => Integer'(0));
      else
         Root.Set_Field (Field_Name => Expires_String,
                         Field      => Integer (Expires_In));
      end if;

      Root.Set_Field (Field_Name => Registered_String,
                      Field      => Object.Registered);
      Root.Set_Field (Field_Name => Image (Object.Peer_ID),
                      Field      => Object.Values);
      return Root;
   end To_JSON;

   ------------------
   --  Unregister  --
   ------------------
   procedure Unregister (Object : out Instance) is
   begin
      if Object.Registered then
         Object.Registered := False;
         Notification.Broadcast
           (Client_Notification.Peer_State (P => Object).To_JSON);
      end if;
   end Unregister;

end Model.Peer;
