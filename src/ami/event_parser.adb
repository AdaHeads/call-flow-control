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

with Ada.Characters.Latin_1;
with Yolk.Log;
with Errors;
package body Event_Parser is
   use Yolk.Log;

   function Hash_Equivalent_Keys (Left, Right : in Event_Keywords)
                               return Boolean is
   begin
      return Left = Right;
   end Hash_Equivalent_Keys;

   function Hash_Function (Key : in Event_Keywords)
                           return Ada.Containers.Hash_Type
   is
   begin
      return Event_Keywords'Pos (Key);
   end Hash_Function;

   function Parse
     (Event_Text : in Unbounded_String)
      return Event_List_Type.Map
   is
      use Errors;

      procedure Insert_Pair (List       : in out Event_List_Type.Map;
                             Key_Text   : in String;
                             Value      : in Unbounded_String);

      procedure Insert_Pair (List       : in out Event_List_Type.Map;
                             Key_Text   : in String;
                             Value      : in Unbounded_String) is
         Found : Boolean := False;
         Key   : Event_Keywords;
      begin
         begin
            Key := Event_Keywords'Value (Key_Text);
            Found := True;
         exception
            when Constraint_Error =>
               Trace (Info, "Unknown Event Keyword: " & Key_Text);
         end;

         if Found then
            --  Inserting the new pair in the list.
            List.Insert (Key =>  Key, New_Item => Value);
         end if;
      end Insert_Pair;

      package Char renames Ada.Characters.Latin_1;

      List            : Event_List_Type.Map;
      Text_Index      : Positive         := 1;
      Text            : Unbounded_String := Event_Text;
      KeyText         : Unbounded_String;
      ValueText       : Unbounded_String;
      Key_Seperator   : constant String  := Char.Colon & Char.Space;
      Value_Seperator : constant String  := Char.CR & Char.LF;
   begin
      loop
         --  Finds the index for the Key_Seperator
         Text_Index := Index (Source => Text,
                              Pattern => Key_Seperator);

         --  Graps the key out of the text, based on Index.
         KeyText := Head (Source => Text, Count => Text_Index - 1);

         --  Slicing the key part away.
         Text := Unbounded_Slice (Source => Text,
                                  Low => Text_Index + 2,
                                  High => Length (Text));

         --  Finds the index for the Value_Seperator.
         Text_Index := Index (Source => Text,
                              Pattern => Value_Seperator);

         --  Graps the Value out of the text, based on Index
         ValueText := Head (Source => Text, Count => Text_Index - 1);

         Insert_Pair (List => List,
                      Key_Text => To_String (KeyText),
                      Value => ValueText);

         --  If we are near the edge, than we are done.
         exit when Text_Index + 1 = Length (Text);
         --              List_Index := List_Index + 1;

         --  Slicing the Value part away from the text.
         Text := Unbounded_Slice (Source => Text,
                                  Low => Text_Index + 2,
                                  High => Length (Text));
      end loop;
      return List;
   exception
      when Err : others =>
         Log_Exception (Err, "Event: [" & To_String (Event_Text) & "]");
         return List;
   end Parse;

   function Try_Get (Events     : in     Event_List_Type.Map;
                     Field_Name : in     Event_Keywords;
                     Value      :    out Unbounded_String) return Boolean
   is
   begin
      if
        Events.Contains (Key => Field_Name)
      then
         Value :=  Events.Element (Field_Name);
         return True;
      else
         Value := To_Unbounded_String (Field_Name'Img & " did not exsist");
         return False;
      end if;
   end Try_Get;
end Event_Parser;
