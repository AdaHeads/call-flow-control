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

--  SQLite implementation of the prepared statements for an organization.

with GNATCOLL.SQL.Exec;

with SQL_Statements.Organization;

package SQL_Prepared_Statements.Organization is

   use GNATCOLL.SQL.Exec;
   use SQL_Statements.Organization;

   ----------------------------------------------------------------------------
   --  Prepared statements for fetching a basic organization without         --
   --  contacts.                                                             --
   ----------------------------------------------------------------------------

   Organization_Midi_Prepared : constant Prepared_Statement
     := Prepare (Query         => Organization_Midi_Query,
                 Auto_Complete => True,
                 Name          => "organization_midi");

   Organization_URI_Midi_Prepared : constant Prepared_Statement
     := Prepare (Query         => Organization_URI_Midi_Query,
                 Auto_Complete => True,
                 Name          => "organization_uri_midi");

   Organization_Mini_Prepared : constant Prepared_Statement
     := Prepare (Query         => Organization_Mini_Query,
                 Auto_Complete => True,
                 Name          => "organization_mini");

   Organization_URI_Mini_Prepared : constant Prepared_Statement
     := Prepare (Query         => Organization_URI_Mini_Query,
                 Auto_Complete => True,
                 Name          => "organization_uri_mini");

   Organizations_Midi_Prepared : constant Prepared_Statement
     := Prepare (Query         => Organizations_Midi_Query,
                 Auto_Complete => True,
                 Name          => "organizations_midi");

   Organizations_Mini_Prepared : constant Prepared_Statement
     := Prepare (Query         => Organizations_Mini_Query,
                 Auto_Complete => True,
                 Name          => "organizations_mini");

   ----------------------------------------------------------------------------
   --  Prepared statements for fetching an organization and all associated   --
   --  contacts.                                                             --
   ----------------------------------------------------------------------------

   Organization_Maxi_Prepared : constant Prepared_Statement
     := Prepare (Query         => Org_Contacts_Query,
                 Auto_Complete => True,
                 Name          => "organization_maxi");

   Organization_URI_Maxi_Prepared : constant Prepared_Statement
     := Prepare (Query         => Org_URI_Contacts_Query,
                 Auto_Complete => True,
                 Name          => "organization_uri_maxi");

end SQL_Prepared_Statements.Organization;
