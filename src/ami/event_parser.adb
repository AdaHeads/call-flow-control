with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Characters.Latin_1;
package body Event_Parser is

   function Parse (Event_Text : in Unbounded_String) return Event_List_Type is
      package Char renames Ada.Characters.Latin_1;
      EventSize : Integer;
      Linetermination : constant String := Char.CR & Char.LF;
   begin
      --  Counting the number of lines
      EventSize := Ada.Strings.Unbounded.Count (Event_Text, Linetermination);
      declare
         List            : Event_List_Type (1 .. EventSize, KeyValue'Range);
         List_Index      : Positive         := 1;
         Text_Index      : Positive         := 1;
         Text            : Unbounded_String := Event_Text;
         KeyText         : Unbounded_String;
         ValueText       : Unbounded_String;
         Key_Seperator   : constant String  := Char.Colon & Char.Space;
         Value_Seperator : constant String  := Char.CR & Char.LF;
      begin
         loop
            Text_Index := Index (Source => Text,
                            Pattern => Key_Seperator);
            KeyText := Head (Source => Text, Count => Text_Index - 1);
            Text := Unbounded_Slice (Source => Text,
                                     Low => Text_Index + 2,
                                     High => Length (Text));

            Text_Index := Index (Source => Text,
                            Pattern => Value_Seperator);
            ValueText := Head (Source => Text, Count => Text_Index - 1);

            List (List_Index, Key) := KeyText;
            List (List_Index, Value) := ValueText;

            exit when Text_Index + 1 = Length (Text);
            List_Index := List_Index + 1;
            Text := Unbounded_Slice (Source => Text,
                                     Low => Text_Index + 2,
                                     High => Length (Text));
         end loop;
         return List;
      end;
   exception
      when Err : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (Err));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (Err));
         Ada.Text_IO.Put_Line ("Event_Parser.Parse");
         declare
            Null_List : Event_List_Type (1 .. 0, KeyValue'Range);
         begin
            return Null_List;
         end;
   end Parse;
end Event_Parser;
