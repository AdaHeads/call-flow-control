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
      use Common;
      use GNATCOLL.JSON;

      JSON : JSON_Value;
   begin
      JSON := Create_Object;

      JSON.Set_Field
        ("organization_url",
         "http://alice.adaheads.com:4242/organization?org_id=");

      JSON.Set_Field
        ("organizations_list_url",
         "http://alice.adaheads.com:4242/organization/list?view=midi");

      Instance.Content (To_JSON_String (JSON.Write));
   end Generate_Document;

end Handlers.Configuration;
