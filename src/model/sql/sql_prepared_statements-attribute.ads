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

with SQL_Prepared_Statements.Configuration,
     SQL_Statements.Attribute;

package SQL_Prepared_Statements.Attribute is

   use GNATCOLL.SQL;
   use GNATCOLL.SQL.Exec;
   use SQL_Statements.Attribute;

   ----------------------------------------------------------------------------
   --  Prepared statement for fetching the attributes of a contact.          --
   ----------------------------------------------------------------------------

   Contact_Attributes_Prepared : constant Prepared_Statement
     := Prepare (Query         => Contact_With_Id_Attributes_Query,
                 Auto_Complete => True,
                 On_Server     => Configuration.On_Server,
                 Name          => "contact_attributes");

   ----------------------------------------------------------------------------
   --  Prepared statement for fetching the attributes of a contact related   --
   --  to a specific organization.                                           --
   ----------------------------------------------------------------------------

   Contact_Organization_Attributes_Prepared : constant Prepared_Statement
     := Prepare (Query         => Contact_Organization_Attributes_Query,
                 Auto_Complete => True,
                 On_Server     => Configuration.On_Server,
                 Name          => "contact_organization_attributes");

end SQL_Prepared_Statements.Attribute;
