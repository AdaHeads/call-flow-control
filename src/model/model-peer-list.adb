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

with System_Messages;
with Ada.Exceptions;

package body Model.Peer.List is
   use System_Messages;

   Peers : aliased Instance;

   function Get (Object   : in Instance;
                 Identity : in Peer.Identification) return Peer.Instance is
   begin
      if not Object.Peer_Map.Contains (Key => Identity) then
         raise Not_Found;
      end if;

      return Object.Peer_Map.Element (Key => Identity);
   end Get;

   function Get_Singleton return Reference is
   begin
      return Peers'Access;
   end Get_Singleton;

   procedure Put (Object   :    out Instance;
                  New_Peer : in     Peer.Instance) is
      Context : constant String := Package_Name & ".Put";

   begin
      Object.Peer_Map.Insert (Key      => New_Peer.Peer_ID,
                              New_Item => New_Peer);
   exception
      when E : others =>
         System_Messages.Information
           (Message => "Failed to add" &
              New_Peer.To_JSON.Write & " Exception: " &
              Ada.Exceptions.Exception_Information (E),
            Context => Context);

   end Put;

   procedure Register (Object   : in out Instance;
                       Identity : in     Peer.Identification;
                       Contact  : in     String;
                       Expiry   : in     Natural) is

      procedure Update (Key     : in     Peer.Identification;
                        Element : in out Peer.Instance);

      procedure Update (Key     : in     Peer.Identification;
                        Element : in out Peer.Instance) is
         pragma Unreferenced (Key);
      begin
         Element.Values.Set_Field ("contact", Contact);
         Element.Bump_Expiry (Expiry);
         Element.Register;
      end Update;

   begin
      Object.Peer_Map.Update_Element
        (Object.Peer_Map.Find (Identity), Update'Access);
   end Register;

   procedure Set_Singleton (Object : in Instance) is
   begin
      Peers := Object;
   end Set_Singleton;

   function To_JSON (Object : in Instance) return JSON_Value is
      use Peer_Storage;
      JSON_List : JSON_Array;
      Root      : constant JSON_Value := Create_Object;
   begin
      for C in Object.Peer_Map.Iterate loop
         Append (JSON_List, Element (C).To_JSON);
      end loop;
      Root.Set_Field ("peers", JSON_List);
      return Root;
   end To_JSON;

   procedure Unregister (Object   : in out Instance;
                         Identity : in     Peer.Identification) is

      procedure Update (Key     : in     Peer.Identification;
                        Element : in out Peer.Instance);

      procedure Update (Key     : in     Peer.Identification;
                        Element : in out Peer.Instance) is
         pragma Unreferenced (Key);
      begin
         Element.Unregister;
      end Update;

   begin
      Object.Peer_Map.Update_Element
        (Object.Peer_Map.Find (Identity), Update'Access);
   end Unregister;

end Model.Peer.List;
