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

   package Util renames Yolk.Utilities;

   type Keys is (Cache_Max_Element_Age,
                 Cache_Size_Contact,
                 Cache_Size_Organization,
                 DB_Host,
                 DB_Name,
                 DB_Password,
                 DB_Port,
                 DB_User,
                 DB2_Host,
                 DB2_Name,
                 DB2_Password,
                 DB2_Port,
                 DB2_User,
                 Handler_Get_Call,
                 Handler_Get_Contact,
                 Handler_Get_Contact_Attributes,
                 Handler_Get_Contact_Full,
                 Handler_Get_Org_Contacts,
                 Handler_Get_Org_Contacts_Attributes,
                 Handler_Get_Org_Contacts_Full,
                 Handler_Get_Organization,
                 Handler_Get_Queue,
                 Handler_Get_Queue_Length,
                 JSON_Size_Large,
                 JSON_Size_Small);

   type Defaults_Array is array (Keys) of
     Ada.Strings.Unbounded.Unbounded_String;

   Default_Values : constant Defaults_Array :=
                      (Cache_Max_Element_Age
                       => Util.TUS ("86_400"),
                       Cache_Size_Contact
                       => Util.TUS ("10_000"),
                       Cache_Size_Organization
                       => Util.TUS ("1_000"),
                       DB_Host
                       => Util.TUS ("pg.adaheads.com"),
                       DB_Name
                       => Util.TUS ("customers"),
                       DB_Password
                       => Util.TUS ("secret"),
                       DB_Port
                       => Util.TUS ("5432"),
                       DB_User
                       => Util.TUS ("alice"),
                       DB2_Host
                       => Util.TUS ("pg2.adaheads.com"),
                       DB2_Name
                       => Util.TUS ("customers"),
                       DB2_Password
                       => Util.TUS ("secret"),
                       DB2_Port
                       => Util.TUS ("5433"),
                       DB2_User
                       => Util.TUS ("alice"),
                       Handler_Get_Call
                       => Util.TUS ("/get/call"),
                       Handler_Get_Contact
                       => Util.TUS ("/get/contact"),
                       Handler_Get_Contact_Attributes
                       => Util.TUS ("/get/contact_attributes"),
                       Handler_Get_Contact_Full
                       => Util.TUS ("/get/contact_full"),
                       Handler_Get_Org_Contacts
                       => Util.TUS ("/get/org_contacts"),
                       Handler_Get_Org_Contacts_Attributes
                       => Util.TUS ("/get/org_contacts_attributes"),
                       Handler_Get_Org_Contacts_Full
                       => Util.TUS ("/get/org_contacts_full"),
                       Handler_Get_Organization
                       => Util.TUS ("/get/organization"),
                       Handler_Get_Queue
                       => Util.TUS ("/get/queue"),
                       Handler_Get_Queue_Length
                       => Util.TUS ("/get/queue_length"),
                       JSON_Size_Large
                       => Util.TUS ("100_000"),
                       JSON_Size_Small
                       => Util.TUS ("10_000"));

   package Config is new Yolk.Config_File_Parser
     (Key_Type            => Keys,
      Defaults_Array_Type => Defaults_Array,
      Defaults            => Default_Values,
      Config_File         => "configuration/alice_config.ini");

end My_Configuration;
