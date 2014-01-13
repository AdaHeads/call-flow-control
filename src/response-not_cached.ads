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

package Response.Not_Cached is

   Package_Name : constant String := "Response.Not_Cached";

   -------------------------
   --  Generate_Response  --
   -------------------------

   generic

      with procedure Generate_Document
        (Response_Object : in out Object);
      --  Generate the JSON document that is delivered to the client. If
      --  Response_Object.Cacheable is set to True, then the JSON document is
      --  cached.

   function Generate_Response
     (Request : in AWS.Status.Data)
      return AWS.Response.Data;
   --   Generate the data that is delivered to the user.

end Response.Not_Cached;
