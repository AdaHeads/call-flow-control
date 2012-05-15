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
with AWS.URL;
with Call_Queue;
with Errors;
with Storage.Read;

package body Request is

   JSON_MIME_Type : constant String := "application/json; charset=utf-8";

   function Build_JSON_Response
     (Status_Data : in AWS.Status.Data;
      Content     : in String)
      return AWS.Response.Data;
   --  Build the response and compress it if the client supports it. Also wraps
   --  JSON string in foo(JSON string) if the
   --      ?callback=foo
   --  GET parameter is present.

   ---------------------------
   --  Build_JSON_Response  --
   ---------------------------

   function Build_JSON_Response
     (Status_Data : in AWS.Status.Data;
      Content     : in String)
      return AWS.Response.Data
   is
      use Ada.Strings;
      use AWS.Messages;
      use AWS.Response;
      use AWS.Status;

      D          : AWS.Response.Data;
      Encoding   : constant Content_Encoding := Preferred_Coding (Status_Data);

      P          : constant AWS.Parameters.List := Parameters (Status_Data);
      Callback   : constant String              :=
                     Fixed.Trim (P.Get ("callback"), Both);
   begin
      if Callback'Length > 0 then
         D := Build (Content_Type  => JSON_MIME_Type,
                     Message_Body  => Callback & "(" & Content & ")",
                     Encoding      => Encoding,
                     Cache_Control => No_Cache);
      else
         D :=  Build (Content_Type  => JSON_MIME_Type,
                      Message_Body  => Content,
                      Encoding      => Encoding,
                      Cache_Control => No_Cache);
      end if;

      return D;
   end Build_JSON_Response;

   ---------------
   --  Contact  --
   ---------------

   function Contact
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use AWS.Status;
      use AWS.URL;
      use Errors;
      use Storage.Read;

      P      : constant AWS.Parameters.List := Parameters (Request);
      Ce_Id  : constant String              := P.Get ("ce_id");
   begin
      return Build_JSON_Response
        (Status_Data => Request,
         Content     => Get_Contact (Ce_Id));

   exception
      when Event : others =>
         return Build_JSON_Response
           (Status_Data => Request,
            Content     => Exception_Handler
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
      use Errors;
      use Storage.Read;

      P      : constant AWS.Parameters.List := Parameters (Request);
      Ce_Id  : constant String              := P.Get ("ce_id");
   begin
      return Build_JSON_Response
        (Status_Data => Request,
         Content     => Get_Contact_Attributes (Ce_Id));

   exception
      when Event : others =>
         return Build_JSON_Response
           (Status_Data => Request,
            Content     => Exception_Handler
              (Event   => Event,
               Message => "Requested resource: " & URL (URI (Request))));
   end Contact_Attributes;

   --------------------
   --  Contact_Tags  --
   --------------------

   function Contact_Tags
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use AWS.Status;
      use AWS.URL;
      use Errors;
      use Storage.Read;

      P      : constant AWS.Parameters.List := Parameters (Request);
      Ce_Id  : constant String              := P.Get ("ce_id");
   begin
      return Build_JSON_Response
        (Status_Data => Request,
         Content     => Get_Contact_Tags (Ce_Id));

   exception
      when Event : others =>
         return Build_JSON_Response
           (Status_Data => Request,
            Content     => Exception_Handler
              (Event   => Event,
               Message => "Requested resource: " & URL (URI (Request))));
   end Contact_Tags;

   --------------------
   --  Org_Contacts  --
   --------------------

   function Org_Contacts
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use AWS.Status;
      use AWS.URL;
      use Errors;
      use Storage.Read;

      P      : constant AWS.Parameters.List := Parameters (Request);
      Org_Id : constant String              := P.Get ("org_id");
   begin
      return Build_JSON_Response
        (Status_Data => Request,
         Content     => Get_Org_Contacts (Org_Id));

   exception
      when Event : others =>
         return Build_JSON_Response
           (Status_Data => Request,
            Content     => Exception_Handler
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
      use Errors;
      use Storage.Read;

      P      : constant AWS.Parameters.List := Parameters (Request);
      Org_Id : constant String              := P.Get ("org_id");
   begin
      return Build_JSON_Response
        (Status_Data => Request,
         Content     => Get_Org_Contacts_Attributes (Org_Id));

   exception
      when Event : others =>
         return Build_JSON_Response
           (Status_Data => Request,
            Content     => Exception_Handler
              (Event   => Event,
               Message => "Requested resource: " & URL (URI (Request))));
   end Org_Contacts_Attributes;

   -------------------------
   --  Org_Contacts_Tags  --
   -------------------------

   function Org_Contacts_Tags
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use AWS.Status;
      use AWS.URL;
      use Errors;
      use Storage.Read;

      P      : constant AWS.Parameters.List := Parameters (Request);
      Org_Id : constant String              := P.Get ("org_id");
   begin
      return Build_JSON_Response
        (Status_Data => Request,
         Content     => Get_Org_Contacts_Tags (Org_Id));

   exception
      when Event : others =>
         return Build_JSON_Response
           (Status_Data => Request,
            Content     => Exception_Handler
              (Event   => Event,
               Message => "Requested resource: " & URL (URI (Request))));
   end Org_Contacts_Tags;

   --------------------
   --  Organization  --
   --------------------

   function Organization
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use AWS.Status;
      use AWS.URL;
      use Errors;
      use Storage.Read;

      P      : constant AWS.Parameters.List := Parameters (Request);
      Org_Id : constant String              := P.Get ("org_id");
   begin
      return Build_JSON_Response
        (Status_Data => Request,
         Content     => Get_Organization (Org_Id));

   exception
      when Event : others =>
         return Build_JSON_Response
           (Status_Data => Request,
            Content     => Exception_Handler
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
        (Status_Data => Request,
         Content     => Call_Queue.Get);
   end Queue;

   --------------------
   --  Queue_Length  --
   --------------------

   function Queue_Length
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
   begin
      return Build_JSON_Response (Status_Data => Request,
                                  Content     => Call_Queue.Length);
   end Queue_Length;

end Request;
