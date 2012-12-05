-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                          Handlers.Organization                            --
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
with View;

package body Handlers.Organization is

   ------------------
   --  Bad_Org_Id  --
   ------------------

   procedure Bad_Org_Id
     (Response_Object :    out Response.Object;
      Message         : in     String)
   is
      use System_Message;
   begin
      Error.Bad_Org_Id_Key (Message         => Message,
                            Response_Object => Response_Object);
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
      O.Get_Full (O_ID => Get_Org_Id (Response_Object));

      if O /= Null_Organization then
         Response_Object.Cacheable (True);
         Response_Object.HTTP_Status_Code (OK);
      else
         Response_Object.HTTP_Status_Code (Not_Found);
      end if;

      Response_Object.Content (O.To_JSON_String (View.Full));
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
        (Parameters (Response_Object.Status_Data).Get ("org_id"));
   end Get_Org_Id;

end Handlers.Organization;
