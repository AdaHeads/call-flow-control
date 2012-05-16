-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                             My_Configuration                              --
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

with Ada.Strings.Unbounded;
with Yolk.Config_File_Parser;
with Yolk.Utilities;

package My_Configuration is

   use Yolk.Utilities;

   type Keys is (Cache_Max_Element_Age,
                 Cache_Size_Contact,
                 Cache_Size_Organization,
                 DB_Host,
                 DB_Name,
                 DB_User,
                 DB_Password,
                 DB2_Host,
                 DB2_Name,
                 DB2_User,
                 DB2_Password,
                 Handler_Get_Contact,
                 Handler_Get_Contact_Attributes,
                 Handler_Get_Org_Contacts,
                 Handler_Get_Org_Contacts_Attributes,
                 Handler_Get_Organization,
                 Handler_Get_Queue,
                 Handler_Get_Queue_Length);

   type Defaults_Array is array (Keys) of
     Ada.Strings.Unbounded.Unbounded_String;

   Default_Values : constant Defaults_Array :=
                      (Cache_Max_Element_Age
                       => TUS ("86_400"),
                       Cache_Size_Contact
                       => TUS ("10_000"),
                       Cache_Size_Organization
                       => TUS ("1_000"),
                       DB_Host
                       => TUS ("pg.adaheads.com"),
                       DB_Name
                       => TUS ("customers"),
                       DB_User
                       => TUS ("alice"),
                       DB_Password
                       => TUS ("secret"),
                       DB2_Host
                       => TUS ("pg2.adaheads.com"),
                       DB2_Name
                       => TUS ("customers"),
                       DB2_User
                       => TUS ("alice"),
                       DB2_Password
                       => TUS ("secret"),
                       Handler_Get_Contact
                       => TUS ("/get/contact"),
                       Handler_Get_Contact_Attributes
                       => TUS ("/get/contact_attributes"),
                       Handler_Get_Org_Contacts
                       => TUS ("/get/org_contacts"),
                       Handler_Get_Org_Contacts_Attributes
                       => TUS ("/get/org_contacts_attributes"),
                       Handler_Get_Organization
                       => TUS ("/get/organization"),
                       Handler_Get_Queue
                       => TUS ("/get/queue"),
                       Handler_Get_Queue_Length
                       => TUS ("/get/queue_length"));

   package Config is new Yolk.Config_File_Parser
     (Key_Type            => Keys,
      Defaults_Array_Type => Defaults_Array,
      Defaults            => Default_Values,
      Config_File         => "configuration/alice_config.ini");

end My_Configuration;
