-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                             SQL_Statements                                --
--                                                                           --
--                                  SPEC                                     --
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

with Database;
with GNATCOLL.SQL.Exec;

package SQL_Statements is

   use GNATCOLL.SQL;
   use GNATCOLL.SQL.Exec;

   package DB renames Database;

   --------------------------------------------------------------------
   --  Statement for fetching a basic organization without contacts  --
   --------------------------------------------------------------------

   Organizations_Query : constant SQL_Query
     := SQL_Select (Fields =>
                      DB.Organization.Full_Name &   --  0
                      DB.Organization.Identifier &  --  1
                      DB.Organization.Json &        --  2
                      DB.Organization.Id,           --  3
                    From => DB.Organization);

   Organization_Query : constant SQL_Query
     := Where_And (Organizations_Query,
                   DB.Organization.Id = Integer_Param (1));

   Organization_Prepared : constant Prepared_Statement
     := Prepare (Query         => Organization_Query,
                 Auto_Complete => True,
                 On_Server     => True,
                 Name          => "organization_basic");

   Organizations_Prepared : constant Prepared_Statement
     := Prepare (Query         => Organizations_Query,
                 Auto_Complete => True,
                 On_Server     => True,
                 Name          => "organizations_basic");

   --------------------------------------------------------------------------
   --  Statement for fetching an organization and all associated contacts  --
   --------------------------------------------------------------------------

   Org_Contacts_Join_1 : constant SQL_Left_Join_Table
     := Left_Join (Full    => DB.Organization,
                   Partial => DB.Organization_Contacts,
                   On      =>
                     DB.Organization_Contacts.FK (DB.Organization));

   Org_Contacts_Join_2 : constant SQL_Left_Join_Table
     := Left_Join (Full    => Org_Contacts_Join_1,
                   Partial => DB.Contact,
                   On      => DB.Organization_Contacts.FK (DB.Contact));

   Org_Contacts_Attributes_Left_Join : constant SQL_Left_Join_Table
     := Left_Join (Full    => Org_Contacts_Join_2,
                   Partial => DB.Contact_Attributes,
                   On      =>
                     DB.Contact_Attributes.FK (DB.Contact));

   Org_Contacts_Query : constant SQL_Query
     := SQL_Select (Fields =>
                      DB.Organization.Full_Name &   --  0
                      DB.Organization.Identifier &  --  1
                      DB.Organization.Json &        --  2
                      DB.Organization.Id &          --  3
                      DB.Contact.Id &               --  4
                      DB.Contact.Full_Name &        --  5
                      DB.Contact.Is_Human &         --  6
                      DB.Contact_Attributes.Json,   --  7
                    From   => Org_Contacts_Attributes_Left_Join,
                    Where  =>
                      DB.Organization.Id = Integer_Param (1)
                    and
                      (DB.Contact_Attributes.Organization_Id =
                                                       Integer_Param (1)
                       or Is_Null
                         (DB.Contact_Attributes.Organization_Id)));

   Org_Contacts_Prepared : constant Prepared_Statement
     := Prepare (Query         => Org_Contacts_Query,
                 Auto_Complete => True,
                 On_Server     => True,
                 Name          => "contacts");

   ------------------------------------------------------------------------
   --  Statement for all contacts associated with an organization. This  --
   --  does not include the organization itself.                         --
   ------------------------------------------------------------------------

   Contacts_Join_1 : constant SQL_Left_Join_Table
     := Join (Table1 => DB.Organization,
              Table2 => DB.Organization_Contacts,
              On     =>
                DB.Organization_Contacts.FK (DB.Organization));

   Contacts_Join_2 : constant SQL_Left_Join_Table
     := Join (Table1 => Contacts_Join_1,
              Table2 => DB.Contact,
              On     => DB.Organization_Contacts.FK (DB.Contact));

   Contacts_Attributes_Left_Join : constant SQL_Left_Join_Table
     := Left_Join (Full    => Contacts_Join_2,
                   Partial => DB.Contact_Attributes,
                   On      =>
                     DB.Contact_Attributes.FK (DB.Contact));

   Contacts_Query : constant SQL_Query
     := SQL_Select (Fields =>
                      DB.Contact.Id &                         --  0
                      DB.Contact.Full_Name &                  --  1
                      DB.Contact.Is_Human &                   --  2
                      DB.Contact_Attributes.Json &            --  3
                      DB.Contact_Attributes.Contact_Id &      --  4
                      DB.Contact_Attributes.Organization_Id,  --  5
                    From   => Contacts_Attributes_Left_Join,
                    Where  =>
                      DB.Organization.Id = Integer_Param (1)
                    and
                      (DB.Contact_Attributes.Organization_Id =
                                                       Integer_Param (1)
                       or Is_Null
                         (DB.Contact_Attributes.Organization_Id)));

   Contacts_Prepared : constant Prepared_Statement
     := Prepare (Query         => Contacts_Query,
                 Auto_Complete => True,
                 On_Server     => True,
                 Name          => "organization_contacts");

   ---------------------------------------------------------------------------
   --  Prepared statement for fetching a contact with ALL its associated    --
   --  attributes, ie. one attribute set for each organization the contact  --
   --  belongs to.                                                          --
   ---------------------------------------------------------------------------

   Contact_Query_Full_Left_Join : constant SQL_Left_Join_Table
     :=  Left_Join (Full    => DB.Contact,
                    Partial => DB.Contact_Attributes,
                    On      =>
                      DB.Contact_Attributes.FK (DB.Contact));

   Contact_Query_Full : constant SQL_Query
     := SQL_Select (Fields =>
                      DB.Contact.Id &                         --  0
                      DB.Contact.Full_Name &                  --  1
                      DB.Contact.Is_Human &                   --  2
                      DB.Contact_Attributes.Json &            --  3
                      DB.Contact_Attributes.Contact_Id &      --  4
                      DB.Contact_Attributes.Organization_Id,  --  5
                    From   => Contact_Query_Full_Left_Join);

   Contact_With_Id_Query_Full : constant SQL_Query
     := Where_And (Query => Contact_Query_Full,
                   Where => DB.Contact.Id = Integer_Param (1));

   Contact_Full_Prepared : constant Prepared_Statement
     := Prepare (Query         => Contact_With_Id_Query_Full,
                 Auto_Complete => True,
                 On_Server     => True,
                 Name          => "contact_full");

   ----------------------------------------------------------------------------
   --  Prepared statement for fetching a contact specified by its relation   --
   --  to an organization. Only the attributes that belong to the specified  --
   --  organization are added to the contact.                                --
   ----------------------------------------------------------------------------

   Contact_Org_Specified_Query : constant SQL_Query
     := Where_And (Query => Contact_Query_Full,
                   Where =>
                     DB.Contact.Id = Integer_Param (1)
                   and
                     DB.Contact_Attributes.Organization_Id =
                       Integer_Param (2));

   Contact_Org_Specified_Prepared : constant Prepared_Statement
     := Prepare (Query         => Contact_Org_Specified_Query,
                 Auto_Complete => True,
                 On_Server     => True,
                 Name          => "contact_org_specified");

   --------------------------------------------------------------------
   --  Prepared statement for fetching the attributes of a contact.  --
   --------------------------------------------------------------------

   Contact_Attributes_Query : constant SQL_Query
     := SQL_Select (Fields =>
                      DB.Contact_Attributes.Contact_Id &       --  0
                      DB.Contact_Attributes.Organization_Id &  --  1
                      DB.Contact_Attributes.Json,              --  2
                    From   => DB.Contact_Attributes);

   Contact_With_Id_Attributes_Query : constant SQL_Query
     := Where_And (Query => Contact_Attributes_Query,
                   Where =>
                     DB.Contact_Attributes.Contact_Id = Integer_Param (1));

   Contact_Attributes_Prepared : constant Prepared_Statement
     := Prepare (Query         => Contact_With_Id_Attributes_Query,
                 Auto_Complete => True,
                 On_Server     => True,
                 Name          => "contact_attributes");

   -------------------------------------------------------------------
   --  Prepared statement for fetching the attributes of a contact  --
   --  related to a specific organization.                          --
   -------------------------------------------------------------------

   Contact_Organization_Attributes_Query : constant SQL_Query
     := Where_And (Query => Contact_Attributes_Query,
                   Where =>
                     DB.Contact_Attributes.Contact_Id = Integer_Param (1)
                   and
                     DB.Contact_Attributes.Organization_Id =
                       Integer_Param (2));

   Contact_Organization_Attributes_Prepared : constant Prepared_Statement
     := Prepare (Query         => Contact_Organization_Attributes_Query,
                 Auto_Complete => True,
                 On_Server     => True,
                 Name          => "contact_organization_attributes");

end SQL_Statements;
