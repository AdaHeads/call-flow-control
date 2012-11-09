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
with Ada.Text_IO;
with AWS.Status;
with GNATCOLL.JSON;
with HTTP_Codes;
with Model.Contact;
with System_Message.Error;

package body Contact is

   ----------------------
   --  Bad_Contact_Id  --
   ----------------------

   procedure Bad_Contact_Id
     (Response_Object :    out Response.Object;
      Message         : in     String)
   is
      use System_Message;
   begin
      Error.Bad_Contact_Id.Notify (Message, Response_Object);
   end Bad_Contact_Id;

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
      use GNATCOLL.JSON;
      use HTTP_Codes;
      use Model;
      use type Model.Contact.Contact_Object;

      package MC renames Model.Contact;

      Attr_Array : JSON_Array;
      Attr_JSON  : JSON_Value;
      C_Id       : Model.Contact_Id;
      Contact    : MC.Contact_Object;
      JSON       : JSON_Value;
   begin
      C_Id := Get_Contact_Id (Response_Object);

      Attr_JSON := Create_Object;
      JSON := Create_Object;

      Contact := MC.Get (C_Id);

      if Contact /= MC.Null_Contact_Entity then
         JSON.Set_Field ("contact_id",
                         Integer (Contact.Id));

         JSON.Set_Field ("full_name",
                         Contact.Full_Name);

         JSON.Set_Field ("is_human",
                         Contact.Is_Human);

         for Elem of Contact.Attributes loop
            Ada.Text_IO.Put ("*");
            Attr_JSON := Elem.Get_JSON;

            if Attr_JSON /= JSON_Null then
               Ada.Text_IO.Put ("#");
               Attr_JSON.Set_Field
                 ("organization_id", Integer (Elem.Get_Organization_Id));

               Attr_JSON.Set_Field
                 ("contact_id", Integer (Elem.Get_Contact_Id));

               Append (Attr_Array, Attr_JSON);
            end if;
         end loop;

         if Length (Attr_Array) > 0 then
            JSON.Set_Field ("attributes", Attr_Array);
         end if;

         Response_Object.Set_Cacheable (True);
         Response_Object.Set_HTTP_Status_Code (OK);
      else
         Response_Object.Set_HTTP_Status_Code (Not_Found);
      end if;

      Response_Object.Set_Content (To_JSON_String (JSON.Write));
   end Generate_Document;

   ----------------------
   --  Get_Contact_Id  --
   ----------------------

   function Get_Contact_Id
     (Response_Object : in Response.Object)
      return Model.Contact_Id
   is
      use AWS.Status;
      use Model;
   begin
      return Contact_Id'Value
        (Parameters (Response_Object.Get_Request).Get ("ce_id"));
   end Get_Contact_Id;

end Contact;
