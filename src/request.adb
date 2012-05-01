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

--  with Ada.Exceptions;
with AWS.Messages;
with AWS.Parameters;
with AWS.URL;
with Call_Queue;
with Errors;
with Storage.Read;

package body Request is

   JSON_MIME_Type : constant String := "application/json";

   function Build_Response
     (Status_Data : in AWS.Status.Data;
      Content     : in String)
      return AWS.Response.Data;
   --  Build the response and compress it if the client supports it.

   ----------------------
   --  Build_Response  --
   ----------------------

   function Build_Response
     (Status_Data : in AWS.Status.Data;
      Content     : in String)
      return AWS.Response.Data
   is
      use AWS.Messages;
      use AWS.Response;
      use AWS.Status;

      Encoding : Content_Encoding := Identity;
   begin
      if Is_Supported (Status_Data, GZip) then
         Encoding := GZip;
         --  GZip is supported by the client.
      end if;

      return Build (Content_Type  => JSON_MIME_Type,
                    Message_Body  => Content,
                    Encoding      => Encoding,
                    Cache_Control => No_Cache);
   end Build_Response;

   --------------
   --  Contact --
   --------------

   function Contact
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use AWS.Status;
      use AWS.URL;
      use Errors;
      use Storage.Read;

      P      : constant AWS.Parameters.List := Parameters (Request);
      Ce_Id  : constant String := P.Get ("ce_id");
   begin
      return Build_Response
        (Status_Data => Request,
         Content     => Contact (Natural'Value (Ce_Id)));

   exception
      when E : Constraint_Error =>
         return Build_Response
           (Status_Data => Request,
            Content     => Exception_Handler
              (Event   => E,
               Message => "Bad GET parameter." &
               " ce_id MUST be a natural integer." &
               " URL: " & URL (URI (Request))));
   end Contact;

   -------------------------
   --  Contact_Attributes --
   -------------------------

   function Contact_Attributes
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use AWS.Status;
      use AWS.URL;
      use Errors;
      use Storage.Read;

      P      : constant AWS.Parameters.List := Parameters (Request);
      Ce_Id  : constant String := P.Get ("ce_id");
   begin
      return Build_Response
        (Status_Data => Request,
         Content     => Contact_Attributes (Natural'Value (Ce_Id)));

   exception
      when E : Constraint_Error =>
         return Build_Response
           (Status_Data => Request,
            Content     => Exception_Handler
              (Event   => E,
               Message => "Bad GET parameter." &
               " ce_id MUST be a natural integer." &
               " URL: " & URL (URI (Request))));
   end Contact_Attributes;

   ---------------
   --  Contacts --
   ---------------

   function Contacts
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use AWS.Status;
      use AWS.URL;
      use Errors;

      P      : constant AWS.Parameters.List := Parameters (Request);
      Org_Id : constant String := P.Get ("org_id");
   begin
      return Build_Response
        (Status_Data => Request,
         Content     => Storage.Read.Contacts (Natural'Value (Org_Id)));

   exception
      when E : Constraint_Error =>
         return Build_Response
           (Status_Data => Request,
            Content     => Exception_Handler
              (Event   => E,
               Message => "Bad GET parameter." &
               " org_id MUST be a natural integer." &
               " URL: " & URL (URI (Request))));
   end Contacts;

   --------------------------
   --  Contacts_Attributes --
   --------------------------

   function Contacts_Attributes
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use AWS.Status;
      use AWS.URL;
      use Errors;
      use Storage.Read;

      P      : constant AWS.Parameters.List := Parameters (Request);
      Org_Id : constant String := P.Get ("org_id");
   begin
      return Build_Response
        (Status_Data => Request,
         Content     => Contacts_Attributes (Natural'Value (Org_Id)));

   exception
      when E : Constraint_Error =>
         return Build_Response
           (Status_Data => Request,
            Content     => Exception_Handler
              (Event   => E,
               Message => "Bad GET parameter." &
               " org_id MUST be a natural integer." &
               " URL: " & URL (URI (Request))));
   end Contacts_Attributes;

   -------------------
   --  Organization --
   -------------------

   function Organization
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use AWS.Status;
      use AWS.URL;
      use Errors;

      P      : constant AWS.Parameters.List := Parameters (Request);
      Org_Id : constant String := P.Get ("org_id");
   begin
      return Build_Response
        (Status_Data => Request,
         Content     => Storage.Read.Organization (Natural'Value (Org_Id)));

   exception
      when E : Constraint_Error =>
         return Build_Response
           (Status_Data => Request,
            Content     => Exception_Handler
              (Event   => E,
               Message => "Bad GET parameter." &
               " org_id MUST be a natural integer." &
               " URL: " & URL (URI (Request))));
   end Organization;

   ------------------------------
   --  Organization_Attributes --
   ------------------------------

   function Organization_Attributes
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use AWS.Status;
      use AWS.URL;
      use Errors;
      use Storage.Read;

      P      : constant AWS.Parameters.List := Parameters (Request);
      Org_Id : constant String := P.Get ("org_id");
   begin
      return Build_Response
        (Status_Data => Request,
         Content     => Organization_Attributes (Natural'Value (Org_Id)));

   exception
      when E : Constraint_Error =>
         return Build_Response
           (Status_Data => Request,
            Content     => Exception_Handler
              (Event   => E,
               Message => "Bad GET parameter." &
               " org_id MUST be a natural integer." &
               " URL: " & URL (URI (Request))));
   end Organization_Attributes;

   ------------
   --  Queue --
   ------------

   function Queue
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use AWS.Status;

      P : constant AWS.Parameters.List := Parameters (Request);
   begin
      if P.Get ("kind") = "length" then
         return Build_Response (Status_Data => Request,
                                Content     => Call_Queue.Length);
      else
         return Build_Response
           (Status_Data => Request,
            Content     => Call_Queue.Get);
      end if;
   end Queue;

end Request;
