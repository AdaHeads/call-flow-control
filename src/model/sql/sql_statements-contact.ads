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

package SQL_Statements.Contact is

   use GNATCOLL.SQL;

   ----------------------------------------------------------------------------
   --  SQL for all contacts associated with an organization. This does not   --
   --  include the organization itself.                                      --
   ----------------------------------------------------------------------------

   Contacts_Join_1 : constant SQL_Left_Join_Table
     := Join (Table1 => DB.Organizations,
              Table2 => DB.Organization_Contacts,
              On     =>
                DB.Organization_Contacts.FK (DB.Organizations));

   Contacts_Join_2 : constant SQL_Left_Join_Table
     := Join (Table1 => Contacts_Join_1,
              Table2 => DB.Contacts,
              On     => DB.Organization_Contacts.FK (DB.Contacts));

   Contacts_Attributes_Left_Join : constant SQL_Left_Join_Table
     := Left_Join (Full    => Contacts_Join_2,
                   Partial => DB.Organization_Contacts,
                   On      =>
                     DB.Organization_Contacts.FK (DB.Contacts));

   Contacts_Query : constant SQL_Query
     := SQL_Select (Fields =>
                      DB.Contacts.ID &                           --  0
                      DB.Contacts.Full_Name &                    --  1
                      DB.Contacts.Contact_Type &                 --  2
                      DB.Organization_Contacts.Attributes &      --  3
                      DB.Organization_Contacts.Contact_ID &      --  4
                      DB.Organization_Contacts.Organization_ID & --  5
                      DB.Organizations.ID,                       --  6
                    From   => Contacts_Attributes_Left_Join,
                    Where  =>
                      DB.Organizations.ID = Integer_Param (1)
                    and
                      (DB.Organization_Contacts.Organization_ID =
                                                       Integer_Param (1)
                       or Is_Null
                         (DB.Organization_Contacts.Organization_ID)));

   ----------------------------------------------------------------------------
   --  SQL for fetching a contact with ALL its associated attributes, ie.    --
   --  one attribute set for each organization the contact belongs to.       --
   ----------------------------------------------------------------------------

   Contact_Query_Full_Left_Join : constant SQL_Left_Join_Table
     :=  Left_Join (Full    => DB.Contacts,
                    Partial => DB.Organization_Contacts,
                    On      =>
                      DB.Organization_Contacts.FK (DB.Contacts));

   Contact_Query_Full : constant SQL_Query
     := SQL_Select (Fields =>
                      DB.Contacts.ID &                          --  0
                      DB.Contacts.Full_Name &                   --  1
                      DB.Contacts.Contact_Type &                --  2
                      DB.Organization_Contacts.Attributes &     --  3
                      DB.Organization_Contacts.Contact_ID &     --  4
                      DB.Organization_Contacts.Organization_ID, --  5
                    From   => Contact_Query_Full_Left_Join);

   Contact_With_ID_Query_Full : constant SQL_Query
     := Where_And (Query => Contact_Query_Full,
                   Where => DB.Contacts.ID = Integer_Param (1));

   ----------------------------------------------------------------------------
   --  SQL for fetching a contact specified by its relation to an            --
   --  organization. Only the attributes that belong to the specified        --
   --  organization are added to the contact.                                --
   ----------------------------------------------------------------------------

   Contact_Org_Specified_Query : constant SQL_Query
     := Where_And (Query => Contact_Query_Full,
                   Where =>
                     DB.Contacts.ID = Integer_Param (1)
                   and
                     DB.Organization_Contacts.Organization_ID =
                       Integer_Param (2));

end SQL_Statements.Contact;
