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
with Ada.Strings.Fixed;
with Ada.Text_IO;

with AMI.Peer;
with AMI.Peer_ID;

package body AMI.Channel_ID is

   function Value (Item : in String) return Sequence_Number;

   package Sequence_Text_IO is
     new Ada.Text_IO.Modular_IO (Sequence_Number);

   ---------
   -- "<" --
   ---------

   function "<" (Left  : in Instance;
                 Right : in Instance) return Boolean is
   begin
      if Left.Is_Null or Right.Is_Null then
         return False;
      elsif Left.Sequence = Right.Sequence then
         return Left.Volatile < Right.Volatile or
           Left.Transition < Right.Transition;
      else
         return Left.Sequence < Right.Sequence;
      end if;
   end "<";

   ---------
   -- "=" --
   ---------

   function "=" (Left  : in Instance;
                 Right : in Instance) return Boolean is
   begin
      if Left.Is_Null or Right.Is_Null then
         return Left.Is_Null and Right.Is_Null;
      elsif
        Left.Sequence   = Right.Sequence   and
        Left.Transition = Right.Transition and
        Left.Volatile   = Right.Volatile   then
         return True;
      end if;

      return False;
   end  "=";

   function Image (Item : in Peer_Name) return String is
   begin
      return To_String (Item);
   end Image;

   function Image (Item : in Instance) return String is
      use Ada.Strings.Unbounded;
      Buffer : Unbounded_String := Null_Unbounded_String;
   begin
      if Item.Is_Null then
         return "<null>";
      end if;

      case Item.Transition is
         when Bridge =>
            Append (Buffer, "Bridge/");
         when Async_Goto =>
            Append (Buffer, "AsyncGoto/");
         when Parked =>
            Append (Buffer, "Parked/");
         when others =>
            null;
      end case;

      Append (Buffer, Technologies'Image (Item.Technology) &
                "/" &
                To_String (Item.Peer) &
                "-" &
                Image (Item.Sequence));

      case Item.Volatile is
         when Zombie =>
            Append (Buffer, "<ZOMBIE>");
         when Masq =>
            Append (Buffer, "<MASQ>");
         when others =>
            null;
      end case;

      return To_String (Buffer);
   end Image;

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

   function Is_Local (Item : in Instance) return Boolean is
   begin
      if
        Peer.List.Contains (Peer_ID.Create ("SIP/" & To_String (Item.Peer)))
      then
         return True;
      else
         return False;
      end if;
   end Is_Local;

   function Temporary (Item : in Instance) return Boolean is
   begin
      return Item.Transition /= None or Item.Volatile /= None;
   end Temporary;

   --------------
   -- Validate --
   --------------

   function Validate (Item : in String) return Boolean is
      Dummy_Channel_ID : Channel_ID.Instance := Empty_Channel;
      pragma Unreferenced (Dummy_Channel_ID);
   begin
      if Item'Length < 3 then
         return False;
      end if;

      Dummy_Channel_ID := Create (Item);
      return True;
   exception
      when Invalid_ID =>
         return False;
   end Validate;

   function Value (Item : in String) return Sequence_Number is
   begin
      return Sequence_Number'Value ("16#" & Item & "#");
   end Value;

   -----------
   -- Value --
   -----------

   function Value (Item : in String) return Instance is
      use Ada.Characters.Handling, Ada.Strings.Fixed;
      use Ada.Strings.Unbounded;

      Null_Key      : constant String := "<null>";
      Bridged_Key   : constant String := "Bridged/";
      Parked_Key    : constant String := "Parked/";
      AsyncGoto_Key : constant String := "AsyncGoto/";

      Zombie_Key    : constant String := "<ZOMBIE>";
      Masq_Key      : constant String := "<MASQ>";

      Peer_Key      : constant String := "/";
      Sequence_Key  : constant String := "-";

      Technology_Index : Natural := 0;
      Volatile_Index   : Natural := 0;
      Peer_Index       : Natural;
      Sequence_Index   : Natural;

      Transition : Transitions := None;
      Technology : Technologies;
      Volatile   : Volatility := None;
      Peer       : Peer_Name;
      Sequence   : Sequence_Number;
   begin
      if Item = Null_Key then
         return Null_Channel_ID;
      end if;

      Volatile_Index := Ada.Strings.Fixed.Index
        (Source  => To_Upper (Item),
         Pattern => To_Upper (Masq_Key),
         Going   => Ada.Strings.Backward);

      --  Search for the next valid item.
      if Volatile_Index = 0 then
         Volatile_Index := Ada.Strings.Fixed.Index
           (Source  => To_Upper (Item),
            Pattern => To_Upper (Zombie_Key),
            Going   => Ada.Strings.Backward);
      else
         Volatile := Masq;
      end if;

      if Volatile_Index > 0 then
         Volatile := Zombie;
      end if;
      Sequence_Index := Ada.Strings.Fixed.Index
        (Source  => Item,
         Pattern => Sequence_Key,
         Going   => Ada.Strings.Backward);

      Sequence := Value (Item (Sequence_Index + 1 .. Sequence_Index + 8));

      if To_Upper (Parked_Key) = To_Upper (Head (Item, Parked_Key'Length)) then
         Transition := Parked;
      elsif To_Upper (AsyncGoto_Key) =
        To_Upper (Head (Item, AsyncGoto_Key'Length)) then
         Transition := Async_Goto;
      elsif To_Upper (Bridged_Key) =
        To_Upper (Head (Item, Bridged_Key'Length)) then
         Transition := Bridge;
      end if;

      if Transition = Parked then
         Technology_Index := Item'First + Parked_Key'Length;
      elsif Transition = Async_Goto then
         Technology_Index := Item'First + AsyncGoto_Key'Length;
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

      return (Is_Null    => False,
              Transition => Transition,
              Technology => Technology,
              Peer       => Peer,
              Sequence   => Sequence,
              Volatile   => Volatile);
   exception
      when Constraint_Error =>
         return (Is_Null => True);
   end Value;

end AMI.Channel_ID;
