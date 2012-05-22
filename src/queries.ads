-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                 Queries                                   --
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

package Queries is

   use Database;
   use GNATCOLL.SQL;
   use GNATCOLL.SQL.Exec;

   --  Get_Contact query and prepared statement.
   Get_Contact : constant SQL_Query
     := SQL_Select (Fields        =>
                      Contactentity.Json &
                      Contactentity.Ce_Id &
                      Contactentity.Ce_Name &
                      Contactentity.Is_Human,
                    Where         =>
                      Contactentity.Ce_Id = Integer_Param (1));

   P_Get_Contact : constant Prepared_Statement
     := Prepare (Query         => Get_Contact,
                 Auto_Complete => True,
                 On_Server     => True,
                 Name          => "get_contact");

   --  Get_Contact_Attributes query and prepared statement.
   Get_Contact_Attributes : constant SQL_Query
     := SQL_Select (Fields =>
                      Contactentity_Attributes.Json &
                      Contactentity_Attributes.Ce_Id &
                      Contactentity_Attributes.Org_Id,
                    Where  =>
                      Contactentity_Attributes.Ce_Id = (Integer_Param (1)));

   P_Get_Contact_Attributes : constant Prepared_Statement
     := Prepare (Query         => Get_Contact_Attributes,
                 Auto_Complete => True,
                 On_Server     => True,
                 Name          => "get_contact_attributes");

   --  Get_Contact_Full query and prepared statement.
   Get_Contact_Full_Join : constant SQL_Left_Join_Table
     :=  Left_Join (Full    =>
                      Contactentity,
                    Partial =>
                      Contactentity_Attributes,
                    On      =>
                      Contactentity.Ce_Id = Contactentity_Attributes.Ce_Id);

   Get_Contact_Full : constant SQL_Query
     := SQL_Select (Fields =>
                      Contactentity.Json &
                      Contactentity.Ce_Id &
                      Contactentity.Ce_Name &
                      Contactentity.Is_Human &
                      Contactentity_Attributes.Json &
                      Contactentity_Attributes.Org_Id &
                      Contactentity_Attributes.Ce_Id,
                    From   => Get_Contact_Full_Join,
                    Where  => Contactentity.Ce_Id = Integer_Param (1));

   P_Get_Contact_Full : constant Prepared_Statement
     := Prepare (Query         => Get_Contact_Full,
                 Auto_Complete => True,
                 On_Server     => True,
                 Name          => "get_contact_full");

   --  Get_Organization query and prepared statement.
   Get_Organization : constant SQL_Query
     :=  SQL_Select (Fields =>
                       Organization.Json &
                       Organization.Org_Id &
                       Organization.Org_Name &
                       Organization.Identifier,
                     Where  =>
                       Organization.Org_Id = Integer_Param (1));

   P_Get_Organization : constant Prepared_Statement
     := Prepare (Query         => Get_Organization,
                 Auto_Complete => True,
                 On_Server     => True,
                 Name          => "get_organization");

   --  Get_Org_Contacts query and prepared statement.
   Get_Org_Contacts_Join : constant SQL_Left_Join_Table
     := Join (Table1 =>
                Contactentity,
              Table2 =>
                Organization_Contactentities,
              On     =>
                Contactentity.Ce_Id = Organization_Contactentities.Ce_Id);

   Get_Org_Contacts : constant SQL_Query
     := SQL_Select (Fields =>
                      Contactentity.Json &
                      Contactentity.Ce_Id &
                      Contactentity.Ce_Name &
                      Contactentity.Is_Human,
                    From   => Get_Org_Contacts_Join,
                    Where  =>
                      Organization_Contactentities.Org_Id = Integer_Param (1));

   P_Get_Org_Contacts : constant Prepared_Statement
     := Prepare (Query         => Get_Org_Contacts,
                 Auto_Complete => True,
                 On_Server     => True,
                 Name          => "get_org_contacts");

   --  Get_Org_Contacts_Attributes query and prepared statement.
   Get_Org_Contacts_Attributes : constant SQL_Query
     := SQL_Select (Fields =>
                      Contactentity_Attributes.Json &
                      Contactentity_Attributes.Org_Id &
                      Contactentity_Attributes.Ce_Id,
                    Where  =>
                      Contactentity_Attributes.Org_Id = Integer_Param (1));

   P_Get_Org_Contacts_Attributes : constant Prepared_Statement
     := Prepare (Query         => Get_Org_Contacts_Attributes,
                 Auto_Complete => True,
                 On_Server     => True,
                 Name          => "get_org_contacts_attributes");

   --  Get_Org_Contacts_Full query and prepared statement.
   Get_Org_Contacts_Full_Join : constant SQL_Left_Join_Table
     := Join (Table1 => Contactentity,
              Table2 => Organization_Contactentities,
              On     =>
                Contactentity.Ce_Id = Organization_Contactentities.Ce_Id);

   Get_Org_Contacts_Full_Left_Join : constant SQL_Left_Join_Table
     := Left_Join (Full    => Get_Org_Contacts_Full_Join,
                   Partial => Contactentity_Attributes,
                   On      =>
                     Contactentity.Ce_Id =  Contactentity_Attributes.Ce_Id);

   Get_Org_Contacts_Full : constant SQL_Query
     := SQL_Select (Fields =>
                      Contactentity.Json &
                      Contactentity.Ce_Id &
                      Contactentity.Ce_Name &
                      Contactentity.Is_Human &
                      Contactentity_Attributes.Json &
                      Contactentity_Attributes.Org_Id &
                      Contactentity_Attributes.Ce_Id,
                    From   => Get_Org_Contacts_Full_Left_Join,
                    Where  =>
                      Organization_Contactentities.Org_Id = Integer_Param (1)
                    and
                      (Contactentity_Attributes.Org_Id = Integer_Param (1)
                       or
                         Is_Null (Contactentity_Attributes.Org_Id)));

   P_Get_Org_Contacts_Full : constant Prepared_Statement
     := Prepare (Query         => Get_Org_Contacts_Full,
                 Auto_Complete => True,
                 On_Server     => True,
                 Name          => "get_org_contacts_full");

end Queries;
