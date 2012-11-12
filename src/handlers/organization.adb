-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                              Organization                                 --
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

with AWS.Status;
with HTTP_Codes;
with Model.Organizations;
with System_Message.Error;

package body Organization is

   ------------------
   --  Bad_Org_Id  --
   ------------------

   procedure Bad_Org_Id
     (Response_Object :    out Response.Object;
      Message         : in     String)
   is
      use System_Message;
   begin
      Error.Bad_Org_Id_Key.Notify (Message, Response_Object);
   end Bad_Org_Id;

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
      use Model.Organizations;

      O : Organization_Object;
   begin
      O := Get (Get_Org_Id (Response_Object));

      if O /= Null_Organization_Object then
         Response_Object.Set_Cacheable (True);
         Response_Object.Set_HTTP_Status_Code (OK);
      else
         Response_Object.Set_HTTP_Status_Code (Not_Found);
      end if;

      Response_Object.Set_Content (O.To_JSON);
   end Generate_Document;

--     -------------------
--     --  Create_JSON  --
--     -------------------
--
--     function Create_JSON
--       (C : in out Cursor)
--        return Common.JSON_String
--     is
--        use Common;
--        use Storage;
--
--        procedure Build_Contact_JSON;
--        --  Build an attribute node for a contact entity.
--
--        A_Row          : Row;
--        Contacts_Array : JSON_Array;
--        Contact_JSON   : JSON_Value;
--        JSON           : JSON_Value;
--
--        --------------------------
--        --  Build_Contact_JSON  --
--        --------------------------
--
--        procedure Build_Contact_JSON
--        is
--        begin
--           Contact_JSON := Create_Object;
--
--           Contact_JSON.Set_Field (To_String (A_Row.Ce_Id_Column_Name),
--                                   A_Row.Ce_Id);
--
--           Contact_JSON.Set_Field (To_String (A_Row.Ce_Name_Column_Name),
--                                   To_String (A_Row.Ce_Name));
--
--           Contact_JSON.Set_Field (To_String (A_Row.Is_Human_Column_Name),
--                                   A_Row.Is_Human);
--
--           if A_Row.Attr_JSON /= JSON_Null then
--              Contact_JSON.Set_Field ("attributes", A_Row.Attr_JSON);
--           end if;
--
--           Append (Contacts_Array, Contact_JSON);
--        end Build_Contact_JSON;
--     begin
--        JSON := Create_Object;
--
--        if C.Has_Row then
--           A_Row := C.Element;
--
--           JSON := A_Row.Org_JSON;
--
--           JSON.Set_Field (To_String (A_Row.Org_Id_Column_Name),
--                           A_Row.Org_Id);
--
--           JSON.Set_Field (To_String (A_Row.Org_Name_Column_Name),
--                           A_Row.Org_Name);
--
--           JSON.Set_Field (To_String (A_Row.Identifier_Column_Name),
--                           A_Row.Identifier);
--
--           Build_Contact_JSON;
--
--           C.Next;
--
--           while C.Has_Row loop
--              --  If we have more than one row, then we have more than one
--              --  contact entity in the organization.
--              A_Row := C.Element;
--              Build_Contact_JSON;
--              C.Next;
--           end loop;
--
--           JSON.Set_Field ("contact", Contacts_Array);
--        end if;
--
--        return To_JSON_String (JSON.Write);
--     end Create_JSON;

   ------------------
   --  Get_Org_Id  --
   ------------------

   function Get_Org_Id
     (Response_Object : in Response.Object)
      return Model.Organization_Identifier
   is
      use AWS.Status;
   begin
      return Model.Organization_Identifier'Value
        (Parameters (Response_Object.Get_Request).Get ("org_id"));
   end Get_Org_Id;

end Organization;
