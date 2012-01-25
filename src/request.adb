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

with AWS.LDAP.Client;
with AWS.Messages;
with AWS.Parameters;
with LDAP.Read;
with My_Configuration;

package body Request is

   use My_Configuration;

   package My renames My_Configuration;

   --  JSON_MIME_Type : constant String := "application/json";
   JSON_MIME_Type : constant String := "text/html";

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
   --  Company --
   --------------

   function Company
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use AWS.LDAP.Client;
      use AWS.Status;

      P : constant AWS.Parameters.List := Parameters (Request);
   begin
      return Build_Response
        (Status_Data => Request,
         Content     => LDAP.Read.Search
           (Base   => My.Config.Get (LDAP_Base_DN),
            Filter => "(&(objectClass=organization)(o=" & P.Get ("o") & "))",
            Scope  => LDAP_Scope_Subtree,
            Attrs  => Attributes ("*")));
   end Company;

   -------------
   --  Person --
   -------------

   function Person
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use AWS.LDAP.Client;
      use AWS.Status;

      P : constant AWS.Parameters.List := Parameters (Request);
   begin
      return Build_Response
        (Status_Data => Request,
         Content     => LDAP.Read.Search
           (Base   => "o=" & P.Get ("o") & "," & My.Config.Get (LDAP_Base_DN),
            Filter => "(&(objectclass=person)(cn=" & P.Get ("cn") & "))",
            Scope  => LDAP_Scope_Subtree,
            Attrs  => Attributes ("*")));
   end Person;

   --------------
   --  Persons --
   --------------

   function Persons
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use AWS.LDAP.Client;
      use AWS.Status;

      P : constant AWS.Parameters.List := Parameters (Request);
   begin
      return Build_Response
        (Status_Data => Request,
         Content     => LDAP.Read.Search
           (Base   => "o=" & P.Get ("o") & "," & My.Config.Get (LDAP_Base_DN),
            Filter => "(objectClass=person)",
            Scope  => LDAP_Scope_Subtree,
            Attrs  => Attributes ("*")));
   end Persons;

end Request;
