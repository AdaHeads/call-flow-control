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

with GNATCOLL.SQL.Exec;

package SQL_Statements.Organization is

   use GNATCOLL.SQL;
   use GNATCOLL.SQL.Exec;

   --------------------------------------------------------------------
   --  Statement for fetching a basic organization without contacts  --
   --------------------------------------------------------------------

   Organizations_Mini_Query : constant SQL_Query
     := SQL_Select (Fields =>
                      DB.Organization.Full_Name &   --  0
                      DB.Organization.Identifier &  --  1
                      DB.Organization.Id,           --  2
                    From   => DB.Organization);

   Organizations_Midi_Query : constant SQL_Query
     := SQL_Select (Fields =>
                      DB.Organization.Full_Name &   --  0
                      DB.Organization.Identifier &  --  1
                      DB.Organization.Json &        --  2
                      DB.Organization.Id,           --  3
                    From => DB.Organization);

   Organization_Midi_Query : constant SQL_Query
     := Where_And (Organizations_Midi_Query,
                   DB.Organization.Id = Integer_Param (1));

   Organization_Mini_Query : constant SQL_Query
     := Where_And (Organizations_Mini_Query,
                   DB.Organization.Id = Integer_Param (1));

   Organization_Midi_Prepared : constant Prepared_Statement
     := Prepare (Query         => Organization_Midi_Query,
                 Auto_Complete => True,
                 On_Server     => True,
                 Name          => "organization_midi");

   Organization_Mini_Prepared : constant Prepared_Statement
     := Prepare (Query         => Organization_Mini_Query,
                 Auto_Complete => True,
                 On_Server     => True,
                 Name          => "organization_mini");

   Organizations_Midi_Prepared : constant Prepared_Statement
     := Prepare (Query         => Organizations_Midi_Query,
                 Auto_Complete => True,
                 On_Server     => True,
                 Name          => "organizations_midi");

   Organizations_Mini_Prepared : constant Prepared_Statement
     := Prepare (Query         => Organizations_Mini_Query,
                 Auto_Complete => True,
                 On_Server     => True,
                 Name          => "organizations_mini");

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

   Organization_Maxi_Prepared : constant Prepared_Statement
     := Prepare (Query         => Org_Contacts_Query,
                 Auto_Complete => True,
                 On_Server     => True,
                 Name          => "organization_maxi");

end SQL_Statements.Organization;
