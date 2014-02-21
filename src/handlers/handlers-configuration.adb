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

with GNATCOLL.JSON;

with Configuration,
     Response.Templates;

package body Handlers.Configuration is

   ----------------
   --  Callback  --
   ----------------

   function Callback return AWS.Response.Callback is
   begin
      return Generate_Response'Access;
   end Callback;

   -------------------------
   --  Generate_Response  --
   -------------------------

   function Generate_Response (Request : AWS.Status.Data)
                               return AWS.Response.Data is
      use Ada.Directories;
      package CFS renames Configuration;
      use GNATCOLL.JSON;

      File_Name : constant String  := CFS.Config.Get (Client_Config_File);
      File_Size : constant Natural := Natural (Size (File_Name));

      subtype JSON_String    is String (1 .. File_Size);
      package JSON_String_IO is new Ada.Direct_IO (JSON_String);

      File        : JSON_String_IO.File_Type;
      JSON_S      : JSON_String;
      JSON_Object : JSON_Value;
   begin
      JSON_String_IO.Open  (File,
                            Mode => JSON_String_IO.In_File,
                            Name => File_Name);
      JSON_String_IO.Read  (File,
                            Item => JSON_S);
      JSON_String_IO.Close (File);

      JSON_Object := GNATCOLL.JSON.Read (JSON_S, "bad bob_configuration JSON");

      return Response.Templates.OK (Request       => Request,
                                    Response_Body => JSON_Object);

   end Generate_Response;

end Handlers.Configuration;
