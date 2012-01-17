-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                  Data                                     --
--                                                                           --
--                                  SPEC                                     --
--                                                                           --
--                     Copyright (C) 2012-, AdaHeads K/S                      --
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
with LDAP_Connection;
--  with My_Configuration;

package Data is
private

   --  package My renames My_Configuration;

   package LDAP is new LDAP_Connection ("alpha.adaheads.com",
                                        "cn=Directory Manager",
                                        "D3nSort3H3st",
                                        1389);

   function To_JSON
     (Directory : in AWS.LDAP.Client.Directory;
      Message   : in AWS.LDAP.Client.LDAP_Message)
      return GNATCOLL.JSON.JSON_Value;
   --  Convert a LDAP message to a JSON object.

end Data;
