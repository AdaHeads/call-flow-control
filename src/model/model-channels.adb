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

   protected body Protected_Channel_List_Type is
      function Contains (Channel_ID : in Channel_ID_Type) return Boolean is
      begin
         return Protected_List.Contains (Channel_ID);
      end Contains;

      function Get (Channel_ID : in Channel_ID_Type) return Channel_Type is
         Channel : constant Channel_Type :=
           Protected_List.Element (Key => Channel_ID);
      begin
         if Channel = Null_Channel then
            raise CHANNEL_NOT_FOUND;
         end if;

         return Channel;
      end Get;

      --  Places the Channel, in the queue.
      procedure Insert (Channel : in Channel_Type) is
      begin
         Protected_List.Insert
           (Key       => Channel.ID,
            New_Item  => Channel);
         System_Messages.Notify
           (Debug, "Inserted channel:" & Channel.To_String);
      exception
         when Constraint_Error =>
            if Protected_List.Element (Key => Channel.ID).ID = Channel.ID then
               raise DUPLICATE_ID with "key " & To_String (Channel.ID) &
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
      procedure Remove (Channel_ID : Channel_ID_Type) is
      begin
         Protected_List.Delete (Channel_ID);
      end Remove;

      function To_String return String is
         use Ada.Strings.Unbounded;
         Item : Unbounded_String;
      begin
         for Channel of Protected_List loop
            Append (Item, Channel.To_String);
         end loop;
         return To_String (Item);
      end To_String;

      procedure Update (Channel : in Channel_Type) is
      begin
         if Protected_List.Contains (Channel.ID) then
            Protected_List.Replace (Key      => Channel.ID,
                                    New_Item => Channel);
         else
            raise CHANNEL_NOT_FOUND;
         end if;
      end Update;
   end Protected_Channel_List_Type;
end Model.Channels;
