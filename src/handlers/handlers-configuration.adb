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

with Ada.Directories;
with Ada.Direct_IO;

with Common;

with GNATCOLL.JSON;

package body Handlers.Configuration is

   ----------------
   --  Callback  --
   ----------------

   function Callback
     return AWS.Dispatchers.Callback.Handler
   is
   begin
      return AWS.Dispatchers.Callback.Create (JSON_Response'Access);
   end Callback;

   -------------------------
   --  Generate_Document  --
   -------------------------

   procedure Generate_Document
     (Instance : in out Response.Object)
   is
      use Ada.Directories;
      use Common;
      use GNATCOLL.JSON;

      File_Name : constant String  := "configuration/bob_configuration.json";
      File_Size : constant Natural := Natural (Size (File_Name));

      subtype JSON_String    is String (1 .. File_Size);
      package JSON_String_IO is new Ada.Direct_IO (JSON_String);

      File        : JSON_String_IO.File_Type;
      JSON        : JSON_String;
      JSON_Object : JSON_Value;
   begin
      JSON_String_IO.Open  (File,
                            Mode => JSON_String_IO.In_File,
                            Name => File_Name);
      JSON_String_IO.Read  (File,
                            Item => JSON);
      JSON_String_IO.Close (File);

      JSON_Object := GNATCOLL.JSON.Read (JSON, "bad bob_configuration JSON");

      Instance.Content (To_JSON_String (JSON_Object.Write));
   end Generate_Document;

end Handlers.Configuration;
