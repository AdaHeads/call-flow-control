-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                            Organization_List                              --
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
with GNATCOLL.JSON;
with HTTP_Codes;
with Model.Organizations;
with System_Message.Error;

package body Organization_List is

   -------------------------------
   --  Bad_List_Kind_Parameter  --
   -------------------------------

   procedure Bad_List_Kind_Parameter
     (Response_Object :    out Response.Object;
      Message         : in     String)
   is
      use System_Message;
   begin
      Error.Bad_List_Kind.Notify (Message, Response_Object);
   end Bad_List_Kind_Parameter;

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
      use Model.Organizations;

      OL : Organization_List_Object;
   begin
      OL.Get;

      if OL.Length > 0 then
         Response_Object.Set_Content (OL.To_JSON_String);

         --  TODO: We need to handle the difference between Basic and Full
         --  views.

         Response_Object.Set_Cacheable (True);
         Response_Object.Set_HTTP_Status_Code (OK);
      else
         Response_Object.Set_HTTP_Status_Code (Not_Found);
      end if;
   end Generate_Document;

   ---------------------
   --  Get_List_Kind  --
   ---------------------

   function Get_List_Kind
     (Response_Object : in Response.Object)
      return List_Type
   is
      use AWS.Status;
   begin
      if Parameters (Response_Object.Get_Request).Count = 0 then
         return Basic;
      end if;

      return List_Type'Value
        (Parameters (Response_Object.Get_Request).Get ("kind"));
   end Get_List_Kind;

end Organization_List;
