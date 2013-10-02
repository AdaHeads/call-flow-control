-------------------------------------------------------------------------------
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

with GNATCOLL.SQL;

package SQL_Statements.Attribute is

   use GNATCOLL.SQL;

   ----------------------------------------------------------------------------
   --  SQL for fetching the attributes of a contact.                         --
   ----------------------------------------------------------------------------

   Contact_Attributes_Query : constant SQL_Query
     := SQL_Select (Fields =>
                      DB.Organization_Contacts.Contact_ID &       --  0
                      DB.Organization_Contacts.Organization_ID &  --  1
                      DB.Organization_Contacts.Attributes,              --  2
                    From   => DB.Organization_Contacts);

   Contact_With_ID_Attributes_Query : constant SQL_Query
     := Where_And (Query => Contact_Attributes_Query,
                   Where =>
                     DB.Organization_Contacts.Contact_ID = Integer_Param (1));

   ----------------------------------------------------------------------------
   --  SQL for fetching the attributes of a contact related to a specific    --
   --  organization.                                                         --
   ----------------------------------------------------------------------------

   Contact_Organization_Attributes_Query : constant SQL_Query
     := Where_And (Query => Contact_Attributes_Query,
                   Where =>
                     DB.Organization_Contacts.Contact_ID = Integer_Param (1)
                   and
                     DB.Organization_Contacts.Organization_ID =
                       Integer_Param (2));

end SQL_Statements.Attribute;
