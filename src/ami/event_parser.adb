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

with Ada.Characters.Latin_1,
     Ada.Exceptions,
     Ada.Text_IO;

with Yolk.Log;

package body Event_Parser is

   function Parse
     (Event_Text : in Unbounded_String)
      return Event_List_Type.Map
   is
      use Yolk.Log;

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

         --  Inserting the new pair in the list.
         List.Insert (Key      => KeyText,
                      New_Item => ValueText);

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
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (Err));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (Err));
         Ada.Text_IO.Put_Line ("Event_Parser.Parse");
         Trace (Error, "Unknown format: " & To_String (Event_Text));
         return List;
   end Parse;
end Event_Parser;
