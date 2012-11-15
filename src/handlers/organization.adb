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
      O.Get_Full (O_Id => Get_Org_Id (Response_Object));

      if O /= Null_Organization_Object then
         Response_Object.Set_Cacheable (True);
         Response_Object.Set_HTTP_Status_Code (OK);
      else
         Response_Object.Set_HTTP_Status_Code (Not_Found);
      end if;

      Response_Object.Set_Content (O.To_JSON_String);
   end Generate_Document;

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
