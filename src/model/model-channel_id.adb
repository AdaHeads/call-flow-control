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

with Ada.Strings.Fixed;

package body Model.Channel_ID is

   ---------
   -- "<" --
   ---------

   function "<" (Left  : in Channel_ID_Type;
                 Right : in Channel_ID_Type) return Boolean is
   begin
      --  if Left.Class = Right.Class then
         return Left.Sequence < Right.Sequence;
      --  end if;
   end "<";

   ---------
   -- "=" --
   ---------

   function "=" (Left  : in Channel_ID_Type;
                 Right : in Channel_ID_Type) return Boolean is
   begin
      --  return (Left.Timestamp = Right.Timestamp) and
      return  (Left.Sequence = Right.Sequence);
   end  "=";

   ------------
   -- Create --
   ------------

   function Create (Item : String) return Channel_ID_Type is
      Class_Offset    : constant Natural := Ada.Strings.Fixed.Index (Pattern => "/",
                                                                     Source  => Item);
      Peername_Offset : constant Natural := Ada.Strings.Fixed.Index (Pattern => "-",
                                                                     Source  => Item,
                                                                     Going   => Ada.Strings.Backward);
   begin
      if Class_Offset < 3 then
         return Null_Channel_ID;
      else
         return
           (Kind => Technology'Value
              (Item (Item'First .. Item'First + Class_Offset - 2)),
            Peername =>
              To_Unbounded_String
                (Item (Item'First + Class_Offset ..
                   Item'First + Peername_Offset - 2)),
            Sequence =>
              (Item (Item'First + Peername_Offset .. Item'Last)));
      end if;
   end Create;

   ---------------
   -- To_String --
   ---------------

   function To_String (Channel_ID : in Channel_ID_Type) return String is
   begin
      if Channel_ID = Null_Channel_ID then
         return "<Null>";
      else
         return
           Technology'Image (Channel_ID.Kind) &
           "/" &
           To_String (Channel_ID.Peername) &
           "-" &
           Channel_ID.Sequence;
      end if;
   end To_String;

   --------------
   -- Validate --
   --------------

   function Validate (Item : in String) return Boolean is
      Dummy_Channel_ID : Channel_ID_Type := Null_Channel_ID;
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

end Model.Channel_ID;
