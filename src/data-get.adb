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

with AWS.LDAP.Client;
with GNATCOLL.JSON;

package body Data.Get is

   ---------------
   --  Company  --
   ---------------

   function Company
     (ID : in String)
      return String
   is
      use AWS.LDAP.Client;
      use GNATCOLL.JSON;

      A_Directory : constant Directory := LDAP.Get_Directory;
      LDAP_MSG    : LDAP_Message;
   begin
      LDAP_MSG := Search
        (A_Directory,
         "dc=example,dc=com",
         "(&(objectClass=organization)(o=" & ID & "))",
         LDAP_Scope_Subtree,
         Attributes ("*"));

      return GNATCOLL.JSON.Write (To_JSON (A_Directory, LDAP_MSG));
   end Company;

   ---------------
   --  Persons  --
   ---------------

   function Persons
     (ID : in String)
      return String
   is
      use AWS.LDAP.Client;
      use GNATCOLL.JSON;

      A_Directory : constant Directory := LDAP.Get_Directory;
      LDAP_MSG    : LDAP_Message;
   begin
      LDAP_MSG := Search
        (A_Directory,
         "o=" & ID & "," & "dc=example,dc=com",
         "(objectClass=person)",
         AWS.LDAP.Client.LDAP_Scope_Subtree,
         Attributes ("*"));

      return GNATCOLL.JSON.Write (To_JSON (A_Directory, LDAP_MSG));
   end Persons;

end Data.Get;
