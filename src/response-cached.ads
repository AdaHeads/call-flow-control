-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                             Response.Cached                               --
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

with AWS.Response;
with AWS.Status;
with Common;

package Response.Cached is

   -------------------------
   --  Generate_Response  --
   -------------------------

   generic

      type Cache_Key_Type is (<>);

      with procedure Bad_Request_Parameters
        (Response_Object :    out Object;
         Message         : in     String);
      --  Response_Object is returned to the client if the request contains bad
      --  GET/POST parameters, ie. if the Get_Cache_Key function have raised a
      --  Constraint_Error exception when trying to do the job of casting
      --  string based HTTP request parameters into whatever Cache_Key_Type
      --  we actually need.

      with function Get_Cache_Key
        (Response_Object : in Object)
         return Cache_Key_Type;
      --  Return the key used to identify an object in a cache.

      with procedure Read_From_Cache
        (Key      : in     Cache_Key_Type;
         Is_Valid :    out Boolean;
         Value    :    out Common.JSON_String);
      --  Find Value in the cache.

      with procedure Generate_Document
        (Response_Object : in out Object);
      --  Generate the JSON document that is delivered to the client. If
      --  Response_Object.Cacheable is set to True, then the JSON document is
      --  cached.

      with procedure Write_To_Cache
        (Key   : in Cache_Key_Type;
         Value : in Common.JSON_String);
      --  Add Key/Value to the cache.

   function Generate_Response
     (Request : in AWS.Status.Data)
      return AWS.Response.Data;
   --   Generate the data that is delivered to the user.

end Response.Cached;
