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
with Model.Contact;
with Request_Parameters;

package body Handlers.Contact is

   -----------------
   --  Cache_Key  --
   -----------------

   function Cache_Key
     (Instance : in Response.Object)
      return Model.Contact_Identifier
   is
      use Model;
   begin
      return Contact_Identifier'Value
        (Instance.Parameter ("ce_id"));
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
     (Instance : in out Response.Object)
   is
      use Common;
      use HTTP_Codes;
      use Model.Contact;

      Contact : Object;
   begin
      Contact := Get (Cache_Key (Instance));

      if Contact /= Null_Object then
         Instance.Is_Cacheable (True);
         Instance.HTTP_Status_Code (OK);
      else
         Instance.HTTP_Status_Code (Not_Found);
      end if;

      Instance.Content (Contact.To_JSON_String);
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
        (Mode           => Request_Parameters.Required,
         Parameter_Name => "ce_id",
         Validate_As    => Request_Parameters.Contact_Identifier);
   end Set_Request_Parameters;

end Handlers.Contact;
