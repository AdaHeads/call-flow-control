-------------------------------------------------------------------------------
--                                                                           --
--                     Copyright (C) 2014-, AdaHeads K/S                     --
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

with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO;

package body Util.Config_File_Parser is

   Values : Defaults_Array_Type := Defaults;

   -----------
   --  Get  --
   -----------

   function Get
     (Key : in Key_Type)
      return Boolean
   is
   begin
      return Boolean'Value (To_String (Values (Key)));
   exception
      when Constraint_Error =>
         raise Conversion_Error with Key_Type'Image (Key);
   end Get;

   -----------
   --  Get  --
   -----------

   function Get
     (Key : in Key_Type)
      return Duration
   is
   begin
      return Duration'Value (To_String (Values (Key)));
   exception
      when Constraint_Error =>
         raise Conversion_Error with Key_Type'Image (Key);
   end Get;

   -----------
   --  Get  --
   -----------

   function Get
     (Key : in Key_Type)
      return Float
   is
   begin
      return Float'Value (To_String (Values (Key)));
   exception
      when Constraint_Error =>
         raise Conversion_Error with Key_Type'Image (Key);
   end Get;

   -----------
   --  Get  --
   -----------

   function Get
     (Key : in Key_Type)
      return Integer
   is
   begin
      return Integer'Value (To_String (Values (Key)));
   exception
      when Constraint_Error =>
         raise Conversion_Error with Key_Type'Image (Key);
   end Get;

   -----------
   --  Get  --
   -----------

   function Get
     (Key : in Key_Type)
      return String
   is
   begin
      return To_String (Values (Key));
   end Get;

   -----------
   --  Get  --
   -----------

   function Get
     (Key : in Key_Type)
      return Unbounded_String
   is
   begin
      return Values (Key);
   end Get;

   ---------------------------
   --  Has_Non_Empty_Value  --
   ---------------------------

   function Has_Non_Empty_Value
     (Key : in Key_Type)
      return Boolean
   is
   begin
      return Values (Key) /= Null_Unbounded_String;
   end Has_Non_Empty_Value;

   -----------------
   --  Load_File  --
   -----------------

   procedure Load_File
     (Config_File : in String)
   is
      use Ada.Strings;
      use Ada.Text_IO;

      File : File_Type;

      ---------------
      --  Get_Key  --
      ---------------

      function Get_Key
        (Line : in String)
         return String;
      --  Find and return the Key part of the Line string. If no Key part is
      --  found, then return an empty string.

      function Get_Key
        (Line : in String)
         return String
      is
         Key_End : Natural;
      begin
         if Line /= "" then
            Key_End := Fixed.Index (Source => Line,
                                    Set    => Maps.To_Set (Space),
                                    Going  => Forward);
            if Key_End > Line'First then
               return Line (Line'First .. Key_End - 1);
            end if;
         end if;

         return Line;
      end Get_Key;

      -----------------
      --  Get_Value  --
      -----------------

      function Get_Value
        (Key   : in String;
         Line  : in String)
         return Unbounded_String;
      --  Find and return the value part of Line as an Unbounded_String. If no
      --  Value part is found, return Null_Unbounded_String.

      function Get_Value
        (Key   : in String;
         Line  : in String)
         return Unbounded_String
      is
      begin
         if Key /= Line then
            return Trim
              (To_Unbounded_String (Line (Key'Last + 2 .. Line'Last)), Left);
         end if;

         return Null_Unbounded_String;
      end Get_Value;

      -------------------------------
      --  Is_Not_Empty_Or_Comment  --
      -------------------------------

      function Is_Not_Empty_Or_Comment
        (Line : in String)
         return Boolean;
      --  Return True if Line contains an actual Key/Value pair, and not just
      --  an empty line or a comment.

      function Is_Not_Empty_Or_Comment
        (Line : in String)
         return Boolean
      is
      begin
         if Line'Length = 0 then
            return False;
         end if;

         if  Line (Line'First .. Line'First) = "#" then
            return False;
         end if;

         if Line'Length > 1
           and then Line (Line'First .. Line'First + 1) = "--"
         then
            return False;
         end if;

         return True;
      end Is_Not_Empty_Or_Comment;
   begin
      Open (File => File,
            Mode => In_File,
            Name => Config_File);

      while not End_Of_File (File => File) loop
         declare
            Line  : constant String := Fixed.Trim (Get_Line (File), Both);
         begin
            if Is_Not_Empty_Or_Comment (Line) then
               declare
                  Key   : constant String := Get_Key (Line);
                  Value : constant Unbounded_String := Get_Value (Key, Line);
               begin
                  Values (Key_Type'Value (Key)) := Value;

               exception
                  when Constraint_Error =>
                     raise Unknown_Key with
                       "Unknown configuration key '" & Key & "' in file "
                         & Config_File;
               end;
            end if;
         end;
      end loop;

      Close (File => File);
   exception
      when Name_Error | Use_Error | Device_Error =>
         raise Cannot_Open_Config_File with Config_File;
   end Load_File;

   procedure Update (Key       : in Key_Type;
                     New_Value : in String) is
   begin
      Values (Key) := To_Unbounded_String (New_Value);
   end Update;

begin

   Load_File (Config_File => Config_File);

end Util.Config_File_Parser;
