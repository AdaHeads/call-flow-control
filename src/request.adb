-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                 Request                                   --
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
with AWS.Messages;
with AWS.Parameters;
with AWS.Response.Set;
with AWS.URL;
with AWS.Utils;
with Cache;
with Call_Queue;
with Common;
with Errors;
with Storage.Read;

package body Request is

   JSON_MIME_Type : constant String := "application/json; charset=utf-8";

   function Build_JSON_Response
     (Request : in AWS.Status.Data;
      Content : in String)
      return AWS.Response.Data;
   --  Build the response and compress it if the client supports it. Also wraps
   --  JSON string in foo(JSON string) if the
   --      ?jsoncallback=foo
   --  GET parameter is present.

   procedure Add_CORS_Headers
     (Request  : in     AWS.Status.Data;
      Response : in out AWS.Response.Data);
   --  If the client sends the Origin: header, add these two CORS headers:
   --    Access-Control-Allow-Origin
   --    Access-Control-Allow-Credentials
   --  where the first one should contain the value of the given Origin: header
   --  and the second a Boolean True. This should be enough to enable very
   --  simple CORS support in Alice.

   function Add_JSONP_Callback
     (Content : in String;
      Request : in AWS.Status.Data)
      return String;
   --  Wrap Content in jsoncallback(Content) if the jsoncallback parameter
   --  is given in the Request. jsonpcallback is replaced with the actual value
   --  of the jsoncallback parameter.
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
     (Content : in String;
      Request : in AWS.Status.Data)
      return String
   is
      use Ada.Strings;
      use AWS.Status;

      P             : constant AWS.Parameters.List := Parameters (Request);
      JSON_Callback : constant String :=
                        Fixed.Trim (P.Get ("jsoncallback"), Both);
   begin
      if JSON_Callback'Length > 0 then
         return JSON_Callback & "(" & Content & ")";
      end if;

      return Content;
   end Add_JSONP_Callback;

   ---------------------------
   --  Build_JSON_Response  --
   ---------------------------

   function Build_JSON_Response
     (Request : in AWS.Status.Data;
      Content : in String)
      return AWS.Response.Data
   is
      use AWS.Messages;
      use AWS.Response;
      use AWS.Status;

      D        : AWS.Response.Data;
      Encoding : constant Content_Encoding := Preferred_Coding (Request);
   begin
      D :=  Build (Content_Type  => JSON_MIME_Type,
                   Message_Body  => Add_JSONP_Callback (Content, Request),
                   Encoding      => Encoding,
                   Cache_Control => No_Cache);

      Add_CORS_Headers (Request, D);

      return D;
   end Build_JSON_Response;

   ------------
   --  Call  --
   ------------

   function Call
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use AWS.Status;
      use AWS.URL;
      use Errors;

      P  : constant AWS.Parameters.List := Parameters (Request);
      Id : constant String              := P.Get ("id");
   begin
      return Build_JSON_Response
        (Request     => Request,
         Content     => Call_Queue.Get_Call (Id));

   exception
      when Event : others =>
         return Build_JSON_Response
           (Request => Request,
            Content => Exception_Handler
              (Event   => Event,
               Message => "Requested resource: " & URL (URI (Request))));
   end Call;

   ---------------
   --  Contact  --
   ---------------

   function Contact
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use AWS.Status;
      use AWS.URL;
      use AWS.Utils;
      use Cache;
      use Common;
      use Errors;

      P     : constant AWS.Parameters.List := Parameters (Request);
      Ce_Id : constant String              := P.Get ("ce_id");

      Valid : Boolean := False;
      Value : JSON_Small.Bounded_String;
   begin
      Contact_Cache.Read (Key      => Ce_Id,
                          Is_Valid => Valid,
                          Value    => Value);

      if not Valid then
         if not Is_Number (Ce_Id) then
            raise GET_Parameter_Error with
              "ce_id must be a valid natural integer";
         end if;

         Value := Storage.Read.Get_Contact (Ce_Id);
      end if;

      return Build_JSON_Response
        (Request => Request,
         Content => JSON_Small.To_String (Value));

   exception
      when Event : others =>
         return Build_JSON_Response
           (Request => Request,
            Content => Exception_Handler
              (Event   => Event,
               Message => "Requested resource: " & URL (URI (Request))));
   end Contact;

   --------------------------
   --  Contact_Attributes  --
   --------------------------

   function Contact_Attributes
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use AWS.Status;
      use AWS.URL;
      use AWS.Utils;
      use Cache;
      use Common;
      use Errors;

      P     : constant AWS.Parameters.List := Parameters (Request);
      Ce_Id : constant String              := P.Get ("ce_id");

      Valid : Boolean := False;
      Value : JSON_Small.Bounded_String;
   begin
      Contact_Attributes_Cache.Read (Key      => Ce_Id,
                                     Is_Valid => Valid,
                                     Value    => Value);

      if not Valid then
         if not Is_Number (Ce_Id) then
            raise GET_Parameter_Error with
              "ce_id must be a valid natural integer";
         end if;

         Value := Storage.Read.Get_Contact_Attributes (Ce_Id);
      end if;

      return Build_JSON_Response
        (Request => Request,
         Content => JSON_Small.To_String (Value));

   exception
      when Event : others =>
         return Build_JSON_Response
           (Request => Request,
            Content => Exception_Handler
              (Event   => Event,
               Message => "Requested resource: " & URL (URI (Request))));
   end Contact_Attributes;

   --------------------
   --  Contact_Full  --
   --------------------

   function Contact_Full
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use AWS.Status;
      use AWS.URL;
      use AWS.Utils;
      use Cache;
      use Common;
      use Errors;

      P     : constant AWS.Parameters.List := Parameters (Request);
      Ce_Id : constant String              := P.Get ("ce_id");

      Valid : Boolean := False;
      Value : JSON_Small.Bounded_String;
   begin
      Contact_Full_Cache.Read (Key      => Ce_Id,
                               Is_Valid => Valid,
                               Value    => Value);

      if not Valid then
         if not Is_Number (Ce_Id) then
            raise GET_Parameter_Error with
              "ce_id must be a valid natural integer";
         end if;

         Value := Storage.Read.Get_Contact_Full (Ce_Id);
      end if;

      return Build_JSON_Response
        (Request => Request,
         Content => JSON_Small.To_String (Value));

   exception
      when Event : others =>
         return Build_JSON_Response
           (Request => Request,
            Content => Exception_Handler
              (Event   => Event,
               Message => "Requested resource: " & URL (URI (Request))));
   end Contact_Full;

   --------------------
   --  Org_Contacts  --
   --------------------

   function Org_Contacts
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use AWS.Status;
      use AWS.URL;
      use AWS.Utils;
      use Cache;
      use Common;
      use Errors;

      P      : constant AWS.Parameters.List := Parameters (Request);
      Org_Id : constant String              := P.Get ("org_id");

      Valid  : Boolean := False;
      Value  : JSON_Large.Bounded_String;
   begin
      Org_Contacts_Cache.Read (Key      => Org_Id,
                               Is_Valid => Valid,
                               Value    => Value);

      if not Valid then
         if not Is_Number (Org_Id) then
            raise GET_Parameter_Error with
              "org_id must be a valid natural integer";
         end if;

         Value := Storage.Read.Get_Org_Contacts (Org_Id);
      end if;

      return Build_JSON_Response
        (Request     => Request,
         Content     => JSON_Large.To_String (Value));

   exception
      when Event : others =>
         return Build_JSON_Response
           (Request => Request,
            Content => Exception_Handler
              (Event   => Event,
               Message => "Requested resource: " & URL (URI (Request))));
   end Org_Contacts;

   -------------------------------
   --  Org_Contacts_Attributes  --
   -------------------------------

   function Org_Contacts_Attributes
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use AWS.Status;
      use AWS.URL;
      use AWS.Utils;
      use Cache;
      use Common;
      use Errors;

      P      : constant AWS.Parameters.List := Parameters (Request);
      Org_Id : constant String              := P.Get ("org_id");

      Valid  : Boolean := False;
      Value  : JSON_Large.Bounded_String;
   begin
      Org_Contacts_Attributes_Cache.Read (Key      => Org_Id,
                                          Is_Valid => Valid,
                                          Value    => Value);

      if not Valid then
         if not Is_Number (Org_Id) then
            raise GET_Parameter_Error with
              "org_id must be a valid natural integer";
         end if;

         Value := Storage.Read.Get_Org_Contacts_Attributes (Org_Id);
      end if;

      return Build_JSON_Response
        (Request => Request,
         Content => JSON_Large.To_String (Value));

   exception
      when Event : others =>
         return Build_JSON_Response
           (Request => Request,
            Content => Exception_Handler
              (Event   => Event,
               Message => "Requested resource: " & URL (URI (Request))));
   end Org_Contacts_Attributes;

   -------------------------
   --  Org_Contacts_Full  --
   -------------------------

   function Org_Contacts_Full
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use AWS.Status;
      use AWS.URL;
      use AWS.Utils;
      use Cache;
      use Common;
      use Errors;

      P      : constant AWS.Parameters.List := Parameters (Request);
      Org_Id : constant String              := P.Get ("org_id");

      Valid  : Boolean := False;
      Value  : JSON_Large.Bounded_String;
   begin
      Org_Contacts_Full_Cache.Read (Key      => Org_Id,
                                    Is_Valid => Valid,
                                    Value    => Value);

      if not Valid then
         if not Is_Number (Org_Id) then
            raise GET_Parameter_Error with
              "org_id must be a valid natural integer";
         end if;

         Value := Storage.Read.Get_Org_Contacts_Full (Org_Id);
      end if;

      return Build_JSON_Response
        (Request => Request,
         Content => JSON_Large.To_String (Value));

   exception
      when Event : others =>
         return Build_JSON_Response
           (Request => Request,
            Content => Exception_Handler
              (Event   => Event,
               Message => "Requested resource: " & URL (URI (Request))));
   end Org_Contacts_Full;

   --------------------
   --  Organization  --
   --------------------

   function Organization
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use AWS.Status;
      use AWS.URL;
      use AWS.Utils;
      use Cache;
      use Common;
      use Errors;

      P      : constant AWS.Parameters.List := Parameters (Request);
      Org_Id : constant String              := P.Get ("org_id");

      Valid  : Boolean := False;
      Value  : JSON_Small.Bounded_String;
   begin
      Organization_Cache.Read (Key      => Org_Id,
                               Is_Valid => Valid,
                               Value    => Value);

      if not Valid then
         if not Is_Number (Org_Id) then
            raise GET_Parameter_Error with
              "org_id must be a valid natural integer";
         end if;

         Value := Storage.Read.Get_Organization (Org_Id);
      end if;

      return Build_JSON_Response
        (Request => Request,
         Content => JSON_Small.To_String (Value));

   exception
      when Event : others =>
         return Build_JSON_Response
           (Request => Request,
            Content => Exception_Handler
              (Event   => Event,
               Message => "Requested resource: " & URL (URI (Request))));
   end Organization;

   -------------
   --  Queue  --
   -------------

   function Queue
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
   begin
      return Build_JSON_Response
        (Request => Request,
         Content => Call_Queue.Get);
   end Queue;

   --------------------
   --  Queue_Length  --
   --------------------

   function Queue_Length
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
   begin
      return Build_JSON_Response (Request => Request,
                                  Content => Call_Queue.Length);
   end Queue_Length;

end Request;
