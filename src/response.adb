-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                Response                                   --
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

with Ada.Strings.Fixed;
with AWS.Response.Set;
with AWS.URL;
with Errors;
with HTTP_Codes;

package body Response is

   JSON_MIME_Type : constant String := "application/json; charset=utf-8";

   procedure Add_CORS_Headers
     (Request  : in     AWS.Status.Data;
      Response : in out AWS.Response.Data)
   with inline;
   --  If the client sends the Origin: header, add these two CORS headers:
   --    Access-Control-Allow-Origin
   --    Access-Control-Allow-Credentials
   --  where the first one should contain the value of the given
   --  Origin : header and the second a Boolean True. This should be enough
   --  to enable very simple CORS support in Alice.

   function Add_JSONP_Callback
     (Content : in Common.JSON_String;
      Request : in AWS.Status.Data)
      return Common.JSON_String
   with inline;
   --  Wrap Content in jsoncallback(Content) if the jsoncallback parameter
   --  is given in the Request. jsonpcallback is replaced with the actual
   --  value of the jsoncallback parameter.
   --  NOTE:
   --  We do not support the callback parameter. It is too generic.

   ------------------------
   --  Add_CORS_Headers  --
   ------------------------

   procedure Add_CORS_Headers
     (Request  : in     AWS.Status.Data;
      Response : in out AWS.Response.Data)
   is
      use AWS.Messages;
      use AWS.Response;
      use AWS.Status;

      Origin_Host : constant String := Origin (Request);
   begin
      if Origin_Host'Length > 0 then
         Set.Add_Header (D     => Response,
                         Name  => Access_Control_Allow_Origin_Token,
                         Value => Origin_Host);

         Set.Add_Header (D     => Response,
                         Name  => Access_Control_Allow_Credentials_Token,
                         Value => "true");
      end if;
   end Add_CORS_Headers;

   --------------------------
   --  Add_JSONP_Callback  --
   --------------------------

   function Add_JSONP_Callback
     (Content : in Common.JSON_String;
      Request : in AWS.Status.Data)
      return Common.JSON_String
   is
      use Ada.Strings;
      use AWS.Status;
      use Common;

      JSON_Callback : constant String := Fixed.Trim
        (Parameters (Request).Get ("jsoncallback"), Both);
   begin
      if JSON_Callback'Length > 0 then
         return To_JSON_String (JSON_Callback)
           & To_JSON_String ("(")
           & Content
           & To_JSON_String (")");
      end if;

      return Content;
   end Add_JSONP_Callback;

   ---------------------------
   --  Build_JSON_Response  --
   ---------------------------

   function Build_JSON_Response
     (Request : in AWS.Status.Data;
      Content : in Common.JSON_String;
      Status  : in AWS.Messages.Status_Code)
      return AWS.Response.Data
   is
      use AWS.Messages;
      use AWS.Response;
      use AWS.Status;
      use Common;

      D        : AWS.Response.Data;
      Encoding : constant Content_Encoding := Preferred_Coding (Request);
   begin
      D :=  Build (Content_Type  => JSON_MIME_Type,
                   Message_Body  =>
                     To_String (Add_JSONP_Callback (Content, Request)),
                   Status_Code   => Status,
                   Encoding      => Encoding,
                   Cache_Control => No_Cache);

      Add_CORS_Headers (Request, D);

      return D;
   end Build_JSON_Response;

   --------------------
   --  Generic_Read  --
   --------------------

   package body Generic_Response is

      ----------------
      --  Generate  --
      ----------------

      function Generate
        (Request : in AWS.Status.Data)
      return AWS.Response.Data
      is
         use AWS.Status;
         use AWS.URL;
         use Common;
         use Errors;
         use HTTP_Codes;

         Cache_Key : Natural;
         Status    : AWS.Messages.Status_Code;
         Valid     : Boolean;
         Value     : JSON_String;
      begin
         Cache_Key := Get_Cache_Key (Request);

         Read_From_Cache (Key      => Cache_Key,
                          Is_Valid => Valid,
                          Value    => Value);

         if Valid then
            Status := OK;
         else
            Query_To_JSON.Generate (Cache_Key => Cache_Key,
                                    Request   => Request,
                                    Status    => Status,
                                    Value     => Value);
         end if;

         return Build_JSON_Response
           (Request => Request,
            Content => Value,
            Status  => Status);

      exception
         when Event : Database_Error =>
            return Build_JSON_Response
              (Request => Request,
               Content => Exception_Handler
                 (Event   => Event,
                  Message => "Requested resource: " & URL (URI (Request))),
               Status  => Server_Error);
         when Event : others =>
            return Build_JSON_Response
              (Request => Request,
               Content => Exception_Handler
                 (Event   => Event,
                  Message => "Requested resource: " & URL (URI (Request))),
               Status  => Bad_Request);
      end Generate;

   end Generic_Response;

   ---------------------
   --  Get_Ce_Id_Key  --
   ---------------------

   function Get_Ce_Id_Key
     (Request : in AWS.Status.Data)
      return Natural
   is
      use AWS.Status;
      use Errors;
   begin
      return Natural'Value (Parameters (Request).Get ("ce_id"));
   exception
      when others =>
         raise GET_Parameter_Error with
           "ce_id must be a valid natural integer";
   end Get_Ce_Id_Key;

   ----------------------
   --  Get_Org_Id_Key  --
   ----------------------

   function Get_Org_Id_Key
     (Request : in AWS.Status.Data)
      return Natural
   is
      use AWS.Status;
      use Errors;
   begin
      return Natural'Value (Parameters (Request).Get ("org_id"));
   exception
      when others =>
         raise GET_Parameter_Error with
           "org_id must be a valid natural integer";
   end Get_Org_Id_Key;

end Response;
