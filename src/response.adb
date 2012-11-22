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

package body Response is

   JSON_MIME_Type : constant String := "application/json; charset=utf-8";

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

   -------------
   --  Build  --
   -------------

   function Build
     (O : in Object)
      return AWS.Response.Data
   is
      use AWS.Messages;
      use AWS.Response;
      use AWS.Status;
      use Common;

      D        : AWS.Response.Data;
      Encoding : constant Content_Encoding := Preferred_Coding (O.Request);
   begin
      D :=  Build (Content_Type  => JSON_MIME_Type,
                   Message_Body  =>
                     To_String (Add_JSONP_Callback
                       (O.Content, O.Request)),
                   Status_Code   => O.HTTP_Status_Code,
                   Encoding      => Encoding,
                   Cache_Control => No_Cache);

      Add_CORS_Headers (O.Request, D);

      return D;
   end Build;

   -----------------
   --  Cacheable  --
   -----------------

   procedure Cacheable
     (O     :    out Object;
      Value : in     Boolean)
   is
   begin
      O.Is_Cacheable := Value;
   end Cacheable;

   ---------------
   --  Content  --
   ---------------

   procedure Content
     (O     :    out Object;
      Value : in     Common.JSON_String)
   is
   begin
      O.Content := Value;
   end Content;

   ---------------
   --  Factory  --
   ---------------

   function Factory
     (Request : in AWS.Status.Data)
      return Object
   is
   begin
      return O : Object do
         O.Request := Request;
      end return;
   end Factory;

   ------------------------
   --  HTTP_Status_Code  --
   ------------------------

   procedure HTTP_Status_Code
     (O     :    out Object;
      Value : in     AWS.Messages.Status_Code)
   is
   begin
      O.HTTP_Status_Code := Value;
   end HTTP_Status_Code;

   -------------------
   --  Request_URL  --
   -------------------

   function Request_URL
     (O : in Object)
      return String
   is
      use AWS.Status;
      use AWS.URL;
   begin
      return URL (URI (O.Request));
   end Request_URL;

   -------------------
   --  Status_Data  --
   -------------------

   function Status_Data
     (O : in Object)
      return AWS.Status.Data
   is
   begin
      return O.Request;
   end Status_Data;

end Response;
