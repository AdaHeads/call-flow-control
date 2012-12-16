-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                Model.Calls                                --
--                                                                           --
--                                  BODY                                     --
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

--  Declares a call list with corresponding primitives.
--  Currently also holds a singleton call list object for easy access.

with Ada.Strings.Unbounded;

with System_Messages;

package body Model.Channels is
   use System_Messages;
   use type Channel.Channel_Type;
   use type Channel_ID.Instance;

   protected body Protected_Channel_List_Type is
      function Contains (Key : in Channel_ID.Instance) return Boolean is
      begin
         return Protected_List.Contains (Key);
      end Contains;

      function Get (Key : in Channel_ID.Instance)
                    return Channel.Channel_Type is
         Item : constant Channel.Channel_Type :=
           Protected_List.Element (Key => Key);
      begin
         if Item = Channel.Null_Channel then
            raise CHANNEL_NOT_FOUND;
         end if;

         return Item;
      end Get;

      --  Places the Channel, in the queue.
      procedure Insert (Item : in Channel.Channel_Type) is
      begin
         Protected_List.Insert
           (Key       => Item.ID,
            New_Item  => Item);
         System_Messages.Notify
           (Debug, "Inserted channel:" & Item.To_String);
      exception
         when Constraint_Error =>
            if Protected_List.Element (Key => Item.ID).ID = Item.ID then
               raise DUPLICATE_ID with "key " & Item.ID.Image &
                 " already in map.";
            end if;
            raise;
      end Insert;

      --  Returns the total number of Channels.
      function Length return Natural is
         use Ada.Containers;
      begin
         return Natural (Protected_List.Length);
      end Length;

      --  Removes the Channel with the specified UniqueID
      procedure Remove (Key : Channel_ID.Instance) is
      begin
         Protected_List.Delete (Key);
      end Remove;

      function To_JSON return GNATCOLL.JSON.JSON_Value is
         use GNATCOLL.JSON;
         Value     : JSON_Value := Create_Object;
         JSON_List : JSON_Array;
         Root      : constant JSON_Value := Create_Object;
      begin
         for Item of Protected_List loop
            if Item /= Channel.Null_Channel then
               Value := Item.To_JSON;
               Append (JSON_List, Value);
            end if;
         end loop;
         Root.Set_Field ("channels", JSON_List);
         return Root;
      end To_JSON;

      function To_String return String is
         use Ada.Strings.Unbounded;
         Item : Unbounded_String;
      begin
         for Channel of Protected_List loop
            Append (Item, Channel.To_String);
         end loop;
         return To_String (Item);
      end To_String;

      procedure Update (Item : in Channel.Channel_Type) is
      begin
         if Protected_List.Contains (Item.ID) then
            Protected_List.Replace (Key      => Item.ID,
                                    New_Item => Item);
         else
            raise CHANNEL_NOT_FOUND;
         end if;
      end Update;
   end Protected_Channel_List_Type;
end Model.Channels;
