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

with Ada.Calendar.Formatting;

with View.Peer;

package body AMI.Peer is

   --  TODO: Add a time limit for when we timeout a peer
   function Available (Peer : in Peer_Type) return Boolean is
   begin
      return Peer /= Null_Peer and Peer.State not in Unknown .. Unregistered;
   end Available;

   function Hash (Peer_ID : in Peer_ID_Type) return Hash_Type is
   begin
      return Ada.Strings.Hash (Peer_ID.To_String);
   end Hash;

   procedure Seen (Peer : out Peer_Type) is
   begin
      Peer.Last_Seen := (Never => False,
                         Time  => Current_Time);
   end Seen;

   function To_JSON (Peer : in Peer_Type)
                     return GNATCOLL.JSON.JSON_Value is
   begin

      return View.Peer.To_JSON (Peer);
   end To_JSON;

   function To_String (Item : in Conditional_Time) return String is
   begin
      if not Item.Never then
         return Ada.Calendar.Formatting.Image (Item.Time);
      else
         return "Never";
      end if;
   end To_String;

   function To_String (Peer : in Peer_Type) return String is
   begin
      return
        "ID => "         & Peer.ID.To_String        & ", " &
        "Agent_ID => "   & Peer.Agent_ID.To_String  & ", " &
        "State => "      & Peer.State'Img           & ", " &
        "Last_State => " & Peer.Last_State'Img      & ", " &
        "Address => "    & To_String (Peer.Address) & ", " &
        "Port => "       & To_String (Peer.Port);
   end To_String;

   protected body Peer_List_Type is
      function Contains (Peer_ID : in Peer_ID_Type) return Boolean is
      begin
         return List.Contains (Key => Peer_ID);
      end Contains;

      function Count return Natural is
      begin
         return Natural (List.Length);
      end Count;

      function Get (Peer_ID : in Peer_ID_Type) return Peer_Type is
      begin
         return List.Element (Key => Peer_ID);
      end Get;

      procedure Put (Peer : in Peer_Type) is
      begin
         if List.Contains (Peer.ID) then
            List.Replace (Key      => Peer.ID,
                          New_Item => Peer);
         else
            List.Insert (Key      => Peer.ID,
                         New_Item => Peer);
         end if;
      end Put;

      function To_JSON return GNATCOLL.JSON.JSON_Value is
         use GNATCOLL.JSON;
         Value     : JSON_Value := Create_Object;
         JSON_List : JSON_Array;
         Root      : constant JSON_Value := Create_Object;
      begin
         for Peer of List loop
            if Peer /= Null_Peer then
               Value := Peer.To_JSON;
               Append (JSON_List, Value);
            end if;
         end loop;
         Root.Set_Field ("peers", JSON_List);
         return Root;
      end To_JSON;

      function To_String return String is
         Item : Unbounded_String;
      begin
         for Peer of List loop
            Append (Item, Peer.To_String);
         end loop;
         return To_String (Item);
      end To_String;
   end Peer_List_Type;

end AMI.Peer;
