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

--  PostgreSQL implementation of the prepared statements for a contact.

with GNATCOLL.SQL.Exec;

with SQL_Prepared_Statements.Configuration,
     SQL_Statements.Contact;

package SQL_Prepared_Statements.Contact is

   use GNATCOLL.SQL.Exec;
   use SQL_Statements.Contact;

   ----------------------------------------------------------------------------
   --  Prepared statement for all contacts associated with an organization.  --
   --  This does not include the organization itself.                        --
   ----------------------------------------------------------------------------

   Contacts_Prepared : constant Prepared_Statement
     := Prepare (Query         => Contacts_Query,
                 Auto_Complete => True,
                 On_Server     => Configuration.On_Server,
                 Name          => "organization_contacts");

   ----------------------------------------------------------------------------
   --  Prepared statement for fetching a contact with ALL its associated     --
   --  attributes, ie. one attribute set for each organization the contact   --
   --  belongs to.                                                           --
   ----------------------------------------------------------------------------

   Contact_Full_Prepared : constant Prepared_Statement
     := Prepare (Query         => Contact_With_Id_Query_Full,
                 Auto_Complete => True,
                 On_Server     => Configuration.On_Server,
                 Name          => "contact_full");

   ----------------------------------------------------------------------------
   --  Prepared statement for fetching a contact specified by its relation   --
   --  to an organization. Only the attributes that belong to the specified  --
   --  organization are added to the contact.                                --
   ----------------------------------------------------------------------------

   Contact_Org_Specified_Prepared : constant Prepared_Statement
     := Prepare (Query         => Contact_Org_Specified_Query,
                 Auto_Complete => True,
                 On_Server     => Configuration.On_Server,
                 Name          => "contact_org_specified");

end SQL_Prepared_Statements.Contact;
