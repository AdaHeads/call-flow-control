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

with AWS.Response;
with AWS.Status;
with Common;
with Yolk.Cache.Discrete_Keys;

package Response.Cached is

   -------------------------
   --  Generate_Response  --
   -------------------------

   generic

      type Cache_Key_Type is (<>);

      with package Cache is new Yolk.Cache.Discrete_Keys
        (Key_Type        => Cache_Key_Type,
         Element_Type    => Common.JSON_String,
         Max_Element_Age => <>);

      with function Cache_Key
        (Instance : in Object)
         return Cache_Key_Type;
      --  Return the key used to identify an object in a cache.

      with procedure Generate_Document
        (Instance : in out Object);
      --  Generate the JSON document that is delivered to the client. If
      --  Instance.Cacheable is set to True, then the JSON document is cached.

      with procedure Set_Request_Parameters
        (Instance : out Object);
      --  Set the required/optional GET/POST request parameters. These are
      --  validated before any content is generated, and if validation fails
      --  then an error is returned to the client.

   function Generate_Response
     (Request : in AWS.Status.Data)
      return AWS.Response.Data;
   --   Generate the data that is delivered to the user.

end Response.Cached;
