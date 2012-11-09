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

with AWS.Status;
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
      use HTTP_Codes;
      use Model.Contact;

      C : Contact_Object;
   begin
      C := Get (Get_Contact_Id (Response_Object));

      if C /= Null_Contact_Object then
         Response_Object.Set_Cacheable (True);
         Response_Object.Set_HTTP_Status_Code (OK);
      else
         Response_Object.Set_HTTP_Status_Code (Not_Found);
      end if;

      Response_Object.Set_Content (C.To_JSON);
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
