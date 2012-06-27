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

--  with AWS.URL;
--  with AWS.Utils;
--  with Cache;
with Ada.Strings.Fixed;
--  with AWS.Parameters;
with AWS.Response.Set;
with AWS.URL;
with AWS.Utils;
--  with Cache;
--  with Call_Queue;
with Errors;
with HTTP_Codes;
--  with Storage.Read;

package body Response is

   JSON_MIME_Type : constant String := "application/json; charset=utf-8";

   function Build_JSON_Response
     (Request     : in AWS.Status.Data;
      Content     : in Common.JSON_String;
      Status_Code : in AWS.Messages.Status_Code)
      return AWS.Response.Data;
   --  Build the response and compress it if the client supports it. Also
   --  wraps JSON string in foo(JSON string) if the
   --      ?jsoncallback=foo
   --  GET parameter is present.

   procedure Add_CORS_Headers
     (Request  : in     AWS.Status.Data;
      Response : in out AWS.Response.Data);
   --  If the client sends the Origin: header, add these two CORS headers:
   --    Access-Control-Allow-Origin
   --    Access-Control-Allow-Credentials
   --  where the first one should contain the value of the given
   --  Origin : header and the second a Boolean True. This should be enough
   --  to enable very simple CORS support in Alice.

   function Add_JSONP_Callback
     (Content : in Common.JSON_String;
      Request : in AWS.Status.Data)
      return Common.JSON_String;
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
   --  Bad_Ce_Id_Parameter  --
   ---------------------------

   function Bad_Ce_Id_Parameter
     (Request : in AWS.Status.Data)
      return Boolean
   is
      use AWS.Status;
      use AWS.Utils;

      Ce_Id : constant String := Parameters (Request).Get ("ce_id");
   begin
      return not Is_Number (Ce_Id);
   end Bad_Ce_Id_Parameter;

   ---------------------------
   --  Build_JSON_Response  --
   ---------------------------

   function Build_JSON_Response
     (Request     : in AWS.Status.Data;
      Content     : in Common.JSON_String;
      Status_Code : in AWS.Messages.Status_Code)
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
                   Status_Code   => Status_Code,
                   Encoding      => Encoding,
                   Cache_Control => No_Cache);

      Add_CORS_Headers (Request, D);

      return D;
   end Build_JSON_Response;

   package body Response_Generic is

      function Get
        (Request : in AWS.Status.Data)
      return AWS.Response.Data
      is
         use AWS.Status;
         use AWS.URL;
         --  use AWS.Utils;
         --  use Cache;
         use Common;
         use Errors;
         use HTTP_Codes;

         Ce_Id       : constant String := Get_Key (Request);
         Status_Code : AWS.Messages.Status_Code;
         Valid       : Boolean;
         Value       : JSON_String;
      begin
         Read_Cache (Key      => Ce_Id,
                     Is_Valid => Valid,
                     Value    => Value);

         if Valid then
            Status_Code := OK;
         else
            if Bad_Parameters (Request) then
               raise GET_Parameter_Error with
                 "ce_id must be a valid natural integer";
            end if;

            Storage_Read (Ce_Id, Status_Code, Value);
         end if;

         return Build_JSON_Response
           (Request     => Request,
            Content     => Value,
            Status_Code => Status_Code);

      exception
         when Event : Database_Error =>
            return Build_JSON_Response
              (Request     => Request,
               Content     => Exception_Handler
                 (Event   => Event,
                  Message => "Requested resource: " & URL (URI (Request))),
               Status_Code => Server_Error);
         when Event : others =>
            return Build_JSON_Response
              (Request     => Request,
               Content     => Exception_Handler
                 (Event   => Event,
                  Message => "Requested resource: " & URL (URI (Request))),
               Status_Code => Bad_Request);
      end Get;

   end Response_Generic;

end Response;
