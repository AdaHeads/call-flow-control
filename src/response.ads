-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                Response                                   --
--                                                                           --
--                                  SPEC                                     --
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

with AWS.Messages;
with AWS.Response;
with AWS.Status;
with Common;
with HTTP_Codes;

package Response is

   type Object is tagged limited private;

   function Build
     (O : in Object)
      return AWS.Response.Data;
   --  Build the response and compress it if the client supports it. Also
   --  wraps JSON string in foo(JSON string) if the
   --      ?jsoncallback=foo
   --  GET parameter is present.

   function Factory
     (Request : in AWS.Status.Data)
      return Object;

   function Get_Request
     (O : in Object)
      return AWS.Status.Data;

   procedure Set_Cacheable
     (O     :    out Object;
      Value : in     Boolean);

   procedure Set_Content
     (O     :    out Object;
      Value : in     Common.JSON_String);

   procedure Set_HTTP_Status_Code
     (O     :    out Object;
      Value : in     AWS.Messages.Status_Code);

   ---------------------------------
   --  Generic_Response_From_SQL  --
   ---------------------------------

   generic

      with function Get_Cache_Key
        (Response_Object : in Object)
      return Natural;
      --  Return the key used to identify an object in a cache.

      with procedure Read_From_Cache
        (Key      : in     Natural;
         Is_Valid :    out Boolean;
         Value    :    out Common.JSON_String);
      --  Find Key in a cache.

      with procedure To_JSON
        (Response_Object : in out Object);
      --  Generate the JSON document that is delivered to the client. If
      --  Cacheable is set to True, then the JSON document can be cached.

      with procedure Write_To_Cache
        (Key   : in Natural;
         Value : in Common.JSON_String);
      --  Add Key/Value to a cache.

   package Generic_Cached_Response is

      function Generate
        (Request : in AWS.Status.Data)
         return AWS.Response.Data;
      --   Generate the data that is delivered to the user.

   end Generic_Cached_Response;

private

   type Object is tagged limited
      record
         Content          : Common.JSON_String := Common.Null_JSON_String;
         HTTP_Status_Code : AWS.Messages.Status_Code := HTTP_Codes.OK;
         Is_Cacheable     : Boolean := False;
         Request          : AWS.Status.Data;
      end record;

end Response;
