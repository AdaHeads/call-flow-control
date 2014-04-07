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

--  This package provides simple access to configuration files.
--  The format is:
--       KEY VALUE
--
--  Comments are prefixed with a # or a --
--
--  # This is a comment
--  -- This is also a comment
--
--  Blank lines and comments are ignored and so is pre-/postfixed whitespace,
--  so this:
--
--  [lots of whitespace]KEY[lots of whitespace]VALUE[lots of whitespace]
--
--  is treated as:
--
--  KEY VALUE
--
--  Values containing whitespace, eg. full sentences and similar, are returned
--  as is. It is not necessary to quote such values, so this:
--
--    KEY some value with whitespace
--
--  is perfectly valid, and will, when calling Get (KEY), return:
--
--    some value with whitespace
--
--  If VALUE is True or False (case-insensitive), then the KEY can be returned
--  as both a String or as a Boolean.
--
--  Trying to convert VALUE to an incompatible type, e.g. a Float to a Boolean
--  will raise the Conversion_Error exception. It will NOT return some dummy
--  value.
--
--  To clear a default value, simply add the key to the configuration file,
--  with no value set. Conversely, you must omit (or comment) keys for which
--  you want to use the default value.

with Ada.Strings.Unbounded;

generic
   use Ada.Strings.Unbounded;

   type Key_Type is (<>);
   type Defaults_Array_Type is array (Key_Type) of Unbounded_String;

   Defaults    : in Defaults_Array_Type;
   Config_File : in String;

package Util.Config_File_Parser is

   Package_Name : constant String := "Util.Config_File_Parser";

   Unknown_Key             : exception;
   --  Is raised when an unknown KEY has been found in the config file.
   Cannot_Open_Config_File : exception;
   --  Is raised when the given config file cannot be opened, eg. due to bad
   --  path.
   Conversion_Error        : exception;
   --  Is raised when a value cannot be converted to a specific type.

   function Get
     (Key : in Key_Type)
      return Boolean;

   function Get
     (Key : in Key_Type)
      return Duration;

   function Get
     (Key : in Key_Type)
      return Float;

   function Get
     (Key : in Key_Type)
      return Integer;

   function Get
     (Key : in Key_Type)
      return String;

   function Get
     (Key : in Key_Type)
      return Unbounded_String;
   --  Get the VALUE for Key and convert it to target type.
   --  Exceptions:
   --    Conversion_Error

   function Has_Non_Empty_Value
     (Key : in Key_Type)
      return Boolean;
   --  Return True if Key is _not_ a Null_Unbounded_String.

   procedure Load_File
     (Config_File : in String := Util.Config_File_Parser.Config_File);
   --  Load the config file Config_File. This can be done over and over as many
   --  times as necessary. The values from the latest file overwrites the
   --  previous values.
   --  Exceptions:
   --    Cannot_Open_Ini_File
   --    Unknown_Key

   procedure Update (Key       : in Key_Type;
                     New_Value : in String);

end Util.Config_File_Parser;
