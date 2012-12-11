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

package body Model.Call_ID is

   function "<" (Left  : in Call_ID_Type;
                 Right : in Call_ID_Type) return Boolean is
   begin
      if Left.Timestamp = Right.Timestamp then
         return Left.Sequence < Right.Sequence;
      else
         return Left.Timestamp < Right.Timestamp;
      end if;
   end "<";

   function "=" (Left  : in Call_ID_Type;
                 Right : in Call_ID_Type) return Boolean is
   begin
      return (Left.Timestamp = Right.Timestamp) and
        (Left.Sequence = Right.Sequence);
   end  "=";

   function Create (Item : String) return Call_ID_Type is
      Offset : constant Natural := Ada.Strings.Fixed.Index (Source => Item, Pattern => ".");
   begin
      return
        (Timestamp => Integer'Value
           (Item (Item'First .. Item'First + Offset - 2)),
         Sequence  => Integer'Value
           (Item (Item'First + Offset .. Item'Last)));

   exception
      when Constraint_Error =>
         raise Invalid_ID with "Bad value: """ & Item & """";

   end Create;

   function To_String (Call_ID : in Call_ID_Type) return String is
   begin
      if Call_ID = Null_Call_ID then
         return "<Null>";
      else
         return Ada.Strings.Fixed.Trim
           (Integer'Image (Call_ID.Timestamp),
            Ada.Strings.Left) &
           "." &
           Ada.Strings.Fixed.Trim
           (Integer'Image (Call_ID.Sequence),
            Ada.Strings.Left);
      end if;
   end To_String;

   function Validate (Item : in String) return Boolean is
      Dummy_Call_ID : Call_ID_Type := Null_Call_ID;
      pragma Unreferenced (Dummy_Call_ID);
   begin
      if Item'Length < 3 then
         return False;
      end if;

      Dummy_Call_ID := Create (Item);
      return True;
   exception
      when Invalid_ID =>
         return False;
   end Validate;

end Model.Call_ID;
