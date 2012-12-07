
-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                               Event_Parser                                --
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

with Ada.Strings.Maps;
with Ada.Strings.Fixed; use Ada.Strings.Fixed; -- For Index
with Ada.Characters.Latin_1;

with System_Messages;   use System_Messages;

package body AMI.Parser is

   function Hash_Equivalent_Keys (Left, Right : in AMI_Key_Type)
                                  return Boolean is
   begin
      return Left = Right;
   end Hash_Equivalent_Keys;

   function Hash_Function (Key : in AMI_Key_Type)
                           return Ada.Containers.Hash_Type
   is
   begin
      return AMI_Key_Type'Pos (Key);
   end Hash_Function;

   function Image (Packet : in Packet_Type) return String is
   begin
      return "Header:" & Image (Packet.Header) & Image (Packet.Fields);
   end Image;

   function Image (List : in Pair_List_Type.Map) return String is
      package Latin_1 renames Ada.Characters.Latin_1;
      Buffer : Unbounded_String;
   begin

      for Cursor in List.Iterate loop
         Append (Buffer,
                 Latin_1.LF &
                   "[" & Pair_List_Type.Key (Cursor)'Img &
                   "] => [" &
                   Pair_List_Type.Element (Cursor) &
                   "]");
      end loop;

      return To_String (Buffer);
   end Image;

   function Image (Item : in Pair_Type) return String is
   begin
      return "[" & AMI_Key_Type'Image (Item.Key) &
        "] => [" & To_String (Item.Value) & "]";
   end Image;

   --  Tokenizes a line into a key-value Pair_Type
   function Parse_Line (Line : in String) return Pair_Type is
      Underscore_Map : constant Ada.Strings.Maps.Character_Mapping
        := Ada.Strings.Maps.To_Mapping ("-", "_");

      Seperator_Index : Natural := Index
        (Source  => Line,
         Pattern => Key_Value_Seperator);
      Key_Length     : Natural;
      Key            : AMI_Key_Type;
   begin
      --  Special cases
      if Line'Length = 0 then
         return Empty_Line;
      elsif Seperator_Index = 0 then
         raise BAD_LINE_FORMAT;
      end if;

      --  Sometimes we get string slice instead of a "real" string.
      if Line'First /= 1 then
         Seperator_Index := Seperator_Index - Line'First + 1;
      end if;

      --  This one really isn't needed, but improves readability of
      --  the source code - hopefully.
      Key_Length := Seperator_Index - Key_Value_Seperator'Length - 1;
      Key        := AMI_Key_Type'Value
        (Translate (Source  => Line (Line'First .. Line'First + Key_Length),
                    Mapping => Underscore_Map));

      --  Return the anonymous object
      return (Key   => Key,
              Value => To_Unbounded_String
                (Line (Line'First + Seperator_Index + 1 .. Line'Last)));
   exception
      when Constraint_Error =>
         System_Messages.Notify
           (System_Messages.Error, "AMI.Parser.Parse_Line: Unknown line """ &
              Line & """");
         return Bad_Line;

      when BAD_LINE_FORMAT =>
         System_Messages.Notify
           (System_Messages.Error,
            "AMI.Parser.Parse_Line: Malformatted line """ &
            Line & """");
         return Bad_Line;
   end Parse_Line;

   function Read_Packet (Client : access AMI.Client.Client_Type)
                         return Packet_Type is
      Current_Pair   : Pair_Type   := Null_Pair;
      Current_Packet : Packet_Type := New_Packet;
   begin
      loop
         Current_Pair := AMI.Parser.Parse_Line
           (Line => Client.Get_Line);

         --  We return on an empty line, meaning the packet is complete
         if Current_Pair = Empty_Line then
            return Current_Packet;

            --  Fill the header
         elsif Current_Packet.Header = Null_Pair then
            Current_Packet.Header := Current_Pair;

            --  Or simply insert a key/value pair
         elsif Current_Pair.Key /= Null_Key then
            Current_Packet.Fields.Insert
              (Key      => Current_Pair.Key,
               New_Item => Current_Pair.Value);
         else
            System_Messages.Notify (Debug, "Read_Packet: Skipping bad line");
         end if;
      end loop;
   end Read_Packet;

   function Try_Get (List  : in     Pair_List_Type.Map;
                     Key   : in     AMI_Key_Type;
                     Value :    out Unbounded_String) return Boolean is
   begin
      if
        List.Contains (Key => Key)
      then
         Value :=  List.Element (Key);
         return True;
      else
         Value := Null_Unbounded_String;
         return False;
      end if;
   end Try_Get;
end AMI.Parser;
