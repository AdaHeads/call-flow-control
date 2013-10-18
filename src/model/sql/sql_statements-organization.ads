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

package SQL_Statements.Organization is

   use GNATCOLL.SQL;

   ----------------------------------------------------------------------------
   --  SQL for fetching a basic organization without contacts                --
   ----------------------------------------------------------------------------

   Organizations_Mini_Query : constant SQL_Query
     := SQL_Select (Fields =>
                      DB.Organizations.Full_Name &  --  0
                      DB.Organizations.URI &        --  1
                      DB.Organizations.ID,          --  2
                    From   => DB.Organizations);

   Organizations_Midi_Query : constant SQL_Query
     := SQL_Select (Fields =>
                      DB.Organizations.Full_Name &  --  0
                      DB.Organizations.URI &        --  1
                      DB.Organizations.Attributes & --  2
                      DB.Organizations.ID,          --  3
                    From => DB.Organizations);

   Organization_Midi_Query : constant SQL_Query
     := Where_And (Organizations_Midi_Query,
                   DB.Organizations.ID = Integer_Param (1));

   Organization_URI_Midi_Query : constant SQL_Query
     := Where_And (Organizations_Midi_Query,
                   DB.Organizations.URI = Text_Param (1));

   Organization_Mini_Query : constant SQL_Query
     := Where_And (Organizations_Mini_Query,
                   DB.Organizations.ID = Integer_Param (1));

   Organization_URI_Mini_Query : constant SQL_Query
     := Where_And (Organizations_Mini_Query,
                   DB.Organizations.URI = Text_Param (1));

   ----------------------------------------------------------------------------
   --  SQL for fetching an organization and all associated contacts          --
   ----------------------------------------------------------------------------

   Org_Contacts_Join_1 : constant SQL_Left_Join_Table
     := Left_Join (Full    => DB.Organizations,
                   Partial => DB.Organization_Contacts,
                   On      =>
                     DB.Organization_Contacts.FK (DB.Organizations));

   Org_Contacts_Join_2 : constant SQL_Left_Join_Table
     := Left_Join (Full    => Org_Contacts_Join_1,
                   Partial => DB.Contacts,
                   On      => DB.Organization_Contacts.FK (DB.Contacts));

   Org_Contacts_Query : constant SQL_Query
     := SQL_Select (Fields =>
                      DB.Organizations.Full_Name &  --  0
                      DB.Organizations.URI &        --  1
                      DB.Organizations.Attributes & --  2
                      DB.Organizations.ID &         --  3
                      DB.Contacts.ID &              --  4
                      DB.Contacts.Full_Name &       --  5
                      DB.Contacts.Contact_Type &           --  6
                      DB.Organization_Contacts.Attributes, --  7
                    From   => Org_Contacts_Join_2,
                    Where  =>
                      DB.Organizations.ID = Integer_Param (1)
                    and
                      (DB.Organization_Contacts.Organization_ID =
                                                       Integer_Param (1)
                       or Is_Null
                         (DB.Organization_Contacts.Organization_ID)));

   Org_URI_Contacts_Query : constant SQL_Query
     := SQL_Select (Fields =>
                      DB.Organizations.Full_Name &         --  0
                      DB.Organizations.URI &               --  1
                      DB.Organizations.Attributes &        --  2
                      DB.Organizations.ID &                --  3
                      DB.Contacts.ID &                     --  4
                      DB.Contacts.Full_Name &              --  5
                      DB.Contacts.Contact_Type &           --  6
                      DB.Organization_Contacts.Attributes, --  7
                    From   => Org_Contacts_Join_2,
                    Where  =>
                      DB.Organizations.URI = Text_Param (1)
                    and
                      (DB.Organization_Contacts.Organization_ID =
                                                       DB.Organizations.ID
                       or Is_Null
                         (DB.Organization_Contacts.Organization_Id)));

end SQL_Statements.Organization;
