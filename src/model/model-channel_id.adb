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

with
  Ada.Characters.Handling,
  Ada.Strings.Fixed,
  Ada.Text_IO;

package body Model.Channel_ID is
   package Sequence_Text_IO is
     new Ada.Text_IO.Modular_IO (Sequence_Number);

   function Image (Item : in Sequence_Number) return String is
      Buffer : String (1 .. 12);
   begin
      Sequence_Text_IO.Put (To   => Buffer,
                            Item => Item,
                            Base => 16);
      for Index in Buffer'Range loop
         if Buffer (Index) = '#' then
            Buffer (Index) := '0';
            exit;
         else
            Buffer (Index) := '0';
         end if;
      end loop;

      return Buffer (4 .. 11);
   end Image;

   function Value (Item : in String) return Sequence_Number is
   begin
      return Sequence_Number'Value ("16#" & Item & "#");
   end Value;

   function "<" (Left  : in Instance;
                 Right : in Instance) return Boolean is
   begin
      if Left.Temporary or Right.Temporary then
         return False;
      else
         return Left.Sequence < Right.Sequence;
      end if;
   end "<";

   function "=" (Left  : in Instance;
                 Right : in Instance) return Boolean is
   begin
      if Left.Temporary or Right.Temporary then
         return False;
      else
         return Left.Sequence = Right.Sequence;
      end if;
   end  "=";

   function Value (Item : in String) return Instance is
      use Ada.Characters.Handling, Ada.Strings.Fixed;

      Null_Key     : constant String := "<null>";
      Parked_Key   : constant String := "Parked/";
      Peer_Key     : constant String := "/";
      Sequence_Key : constant String := "-";

      Technology_Index : Positive;
      Peer_Index       : Natural;
      Sequence_Index   : Natural;

      Parked     : Boolean;
      Technology : Technologies;
      Peer       : Peer_Name;
      Sequence   : Sequence_Number;
   begin
      if Item = Null_Key then
         return Null_Channel_ID;
      end if;

      Sequence_Index := Ada.Strings.Fixed.Index
                          (Source  => Item,
                           Pattern => Sequence_Key,
                           Going   => Ada.Strings.Backward);

      if Sequence_Index + 8 = Item'Last then
         Sequence := Value (Item (Sequence_Index + 1 .. Item'Last));
      else
         return (Temporary => True);
      end if;

      Parked := To_Upper (Parked_Key) = To_Upper (Head (Item, Parked_Key'Length));

      if Parked then
         Technology_Index := Item'First + Parked_Key'Length;
      else
         Technology_Index := Item'First;
      end if;

      Peer_Index := Ada.Strings.Fixed.Index
                      (Source  => Item (Technology_Index .. Sequence_Index),
                       Pattern => Peer_Key);

      Technology := Technologies'Value
                      (Item (Technology_Index .. Peer_Index - 1));

      Peer := To_Unbounded_String
                (Item (Peer_Index + 1 .. Sequence_Index - 1));

      return (Temporary  => False,
              Parked     => Parked,
              Technology => Technology,
              Peer       => Peer,
              Sequence   => Sequence);
   exception
      when Constraint_Error =>
         return (Temporary => True);
   end Value;

   function Image (Item : in Instance) return String is
   begin
      if Item = Null_Channel_ID then
         return "<null>";
      else
         case Item.Temporary is
            when False =>
               if Item.Parked then
                  return "Parked/" &
                         Technologies'Image (Item.Technology) &
                         "/" &
                         To_String (Item.Peer) &
                         "-" &
                         Image (Item.Sequence);
               else
                  return Technologies'Image (Item.Technology) &
                         "/" &
                         To_String (Item.Peer) &
                         "-" &
                         Image (Item.Sequence);
               end if;
            when True =>
               return "<temporary>";
         end case;
      end if;
   end Image;
end Model.Channel_ID;
