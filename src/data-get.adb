-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                 Data.Get                                  --
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

with Ada.Exceptions;
with AWS.LDAP.Client;
with GNATCOLL.JSON;

package body Data.Get is

   function Error_Handler
     (Event       : in Ada.Exceptions.Exception_Occurrence;
      LDAP_Values : in String)
      return String;
   --  Handler exceptions raised due to bad LDAP search parameters.

   ---------------
   --  Company  --
   ---------------

   function Company
     (Organization : in String)
      return String
   is
      use AWS.LDAP.Client;
      use GNATCOLL.JSON;

      A_Directory : constant Directory := LDAP.Get_Directory;
      LDAP_MSG    : LDAP_Message;
   begin
      LDAP_MSG := Search
        (A_Directory,
         My.Config.Get (LDAP_Base_DN),
         "(&(objectClass=organization)(o=" & Organization & "))",
         LDAP_Scope_Subtree,
         Attributes ("*"));

      return Write (To_JSON (A_Directory, LDAP_MSG));

   exception
      when Event : others =>
         return Error_Handler
           (Event, "o=" & Organization);
   end Company;

   ---------------------
   --  Error_Handler  --
   ---------------------

   function Error_Handler
     (Event       : in Ada.Exceptions.Exception_Occurrence;
      LDAP_Values : in String)
      return String
   is
      use Ada.Exceptions;
      use GNATCOLL.JSON;

      JSON_Object : constant JSON_Value := Create_Object;
   begin
      JSON_Object.Set_Field (Field_Name => "error",
                             Field      => Exception_Message (Event));
      JSON_Object.Set_Field (Field_Name => "parameters",
                             Field      => LDAP_Values);
      return Write (JSON_Object);
   end Error_Handler;

   --------------
   --  Person  --
   --------------

   function Person
     (Organization : in String;
      Common_Name  : in String)
      return String
   is
      use AWS.LDAP.Client;
      use GNATCOLL.JSON;

      A_Directory : constant Directory := LDAP.Get_Directory;
      LDAP_MSG    : LDAP_Message;
   begin
      LDAP_MSG := Search
        (A_Directory,
         "o=" & Organization & "," & My.Config.Get (LDAP_Base_DN),
         "(&(objectclass=person)(cn=" & Common_Name & "))",
         AWS.LDAP.Client.LDAP_Scope_Subtree,
         Attributes ("*"));

      return Write (To_JSON (A_Directory, LDAP_MSG));

   exception
      when Event : others =>
         return Error_Handler
           (Event, "o=" & Organization & ", cn=" & Common_Name);
   end Person;

   ---------------
   --  Persons  --
   ---------------

   function Persons
     (Organization : in String)
      return String
   is
      use AWS.LDAP.Client;
      use GNATCOLL.JSON;

      A_Directory : constant Directory := LDAP.Get_Directory;
      LDAP_MSG    : LDAP_Message;
   begin
      LDAP_MSG := Search
        (A_Directory,
         "o=" & Organization & "," & My.Config.Get (LDAP_Base_DN),
         "(objectClass=person)",
         AWS.LDAP.Client.LDAP_Scope_Subtree,
         Attributes ("*"));

      return Write (To_JSON (A_Directory, LDAP_MSG));

   exception
      when Event : others =>
         return Error_Handler
           (Event, "o=" & Organization);
   end Persons;

end Data.Get;
