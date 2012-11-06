-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                 Contact                                   --
--                                                                           --
--                                  BODY                                     --
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

with Ada.Strings.Unbounded;
with AWS.Status;
with GNATCOLL.JSON;
with HTTP_Codes;
with Model.Contact;
with System_Message.Error;

package body Contact is

   ---------------------
   --  Bad_Ce_Id_Key  --
   ---------------------

   procedure Bad_Ce_Id_Key
     (Response_Object :    out Response.Object;
      Message         : in     String)
   is
      use System_Message;
   begin
      Error.Bad_Ce_Id_Key.Notify (Message, Response_Object);
   end Bad_Ce_Id_Key;

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
      use Ada.Strings.Unbounded;
      use Common;
      use GNATCOLL.JSON;
      use HTTP_Codes;

      Attr_Array         : JSON_Array;
      Attr_JSON          : JSON_Value;
      Elements_Processed : Natural := 0;
      JSON               : JSON_Value;

      procedure Build_JSON
        (Element : in Model.Contact.Contact_Entity);
      --  TODO: Write comment;

      ------------------
      --  Build_JSON  --
      ------------------

      procedure Build_JSON
        (Element : in Model.Contact.Contact_Entity)
      is
      begin
         if Elements_Processed = 0 then
            JSON.Set_Field (To_String (Element.Ce_Id_Column_Name),
                            Element.Ce_Id);

            JSON.Set_Field (To_String (Element.Ce_Name_Column_Name),
                            Element.Ce_Name);

            JSON.Set_Field (To_String (Element.Is_Human_Column_Name),
                            Element.Is_Human);
         end if;

         if Element.Attr_JSON /= JSON_Null then
            Attr_JSON := Element.Attr_JSON;

            Attr_JSON.Set_Field
              (To_String (Element.Attr_Org_Id_Column_Name),
               Element.Attr_Org_Id);

            Append (Attr_Array, Attr_JSON);
         end if;

         Elements_Processed := Elements_Processed + 1;
      end Build_JSON;
   begin
      JSON := Create_Object;

      Model.Contact.For_Each
        (Ce_Id   => Get_Ce_Id_Key (Response_Object),
         Process => Build_JSON'Access);

      if Length (Attr_Array) > 0 then
         JSON.Set_Field ("attributes", Attr_Array);
      end if;

      if Elements_Processed > 0 then
         Response_Object.Set_Cacheable (True);
         Response_Object.Set_HTTP_Status_Code (OK);
      else
         Response_Object.Set_HTTP_Status_Code (Not_Found);
      end if;

      Response_Object.Set_Content (To_JSON_String (JSON.Write));
   end Generate_Document;

   ---------------------
   --  Get_Ce_Id_Key  --
   ---------------------

   function Get_Ce_Id_Key
     (Response_Object : in Response.Object)
      return Model.Contactentity_Id
   is
      use AWS.Status;
   begin
      return Model.Contactentity_Id'Value
        (Parameters (Response_Object.Get_Request).Get ("ce_id"));
   end Get_Ce_Id_Key;

end Contact;
