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

with HTTP_Codes;
with Model.Organization;

package body Handlers.Organization is

   -----------------
   --  Cache_Key  --
   -----------------

   function Cache_Key
     (Response_Object : in Response.Object)
      return Model.Organization_Identifier
   is
   begin
      return Model.Organization_Identifier'Value
        (Response_Object.Parameter ("org_id"));
   end Cache_Key;

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
     (Response_Object : in out Response.Object)
   is
      use Common;
      use HTTP_Codes;
      use Model.Organization;

      Organization : Object;
   begin
      Organization := Get (Cache_Key (Response_Object), Mode => Maxi);

      if Organization /= Null_Organization then
         Response_Object.Is_Cacheable (True);
         Response_Object.HTTP_Status_Code (OK);
      else
         Response_Object.HTTP_Status_Code (Not_Found);
      end if;

      Response_Object.Content (Organization.To_JSON_String);
   end Generate_Document;

   ------------------------------
   --  Set_Request_Parameters  --
   ------------------------------

   procedure Set_Request_Parameters
     (Instance : out Response.Object)
   is
      use Response;
   begin
      Instance.Register_Request_Parameter
        (Mode           => Required,
         Parameter_Name => "org_id",
         Validate_As    => Organization_Identifier);
   end Set_Request_Parameters;

end Handlers.Organization;
