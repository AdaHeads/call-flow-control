with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Characters.Latin_1;
package body Event_Parser is

   --  TODO, remove it, for the use of an build-in procedure.
   function CountLines (Event : in Unbounded_String) return Natural is
      count : Integer := 0;
      package Char renames Ada.Characters.Latin_1;

      Event_Temp : constant String := To_String (Event);
   begin
      for i in Event_Temp'Range loop
         if Event_Temp (i) = Char.LF then
            count := count + 1;
         end if;
      end loop;
      return count;
   exception
      when others =>
         Ada.Text_IO.Put_Line ("Something is wrong in Even_parser CountLines");
         return 0;
   end CountLines;

   function Parse (Event_Text : in Unbounded_String) return Event_List_Type is
      package Char renames Ada.Characters.Latin_1;
      EventSize : Integer;
   begin
--        Ada.Text_IO.Put_Line ("Parse: " & To_String (Event_Text));
      EventSize := CountLines (Event_Text);
      declare
         List : Event_List_Type (1 .. EventSize, KeyValue'Range);
         List_Index : Positive := 1;
         Text_Index : Positive := 1;
         Text : Unbounded_String := Event_Text;
         KeyText : Unbounded_String;
         ValueText : Unbounded_String;
         Key_Seperator : constant String := Char.Colon & Char.Space;
         Value_Seperator : constant String := Char.CR & Char.LF;
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

            --              Put_Line ("Key: [" & To_String (Key) & "]");
            --              Put_Line ("Value: [" & To_String (Value) & "]");
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
