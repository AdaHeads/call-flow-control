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

   ----------------
   --  Generate  --
   ----------------

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

   function Generate
     (Request : in AWS.Status.Data)
      return AWS.Response.Data;
   --   Generate the data that is delivered to the user.

end Response.Cached;
