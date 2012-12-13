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

with GNATCOLL.SQL; use GNATCOLL.SQL;
package Database_Names is
   pragma Style_Checks (Off);
   TC_Contact : aliased constant String := "contact";
   Ta_Contact : constant Cst_String_Access := TC_Contact'Access;
   TC_Contact_Attributes : aliased constant String := "contact_attributes";
   Ta_Contact_Attributes : constant Cst_String_Access := TC_Contact_Attributes'Access;
   TC_Contact_Recipients : aliased constant String := "contact_recipients";
   Ta_Contact_Recipients : constant Cst_String_Access := TC_Contact_Recipients'Access;
   TC_Organization : aliased constant String := "organization";
   Ta_Organization : constant Cst_String_Access := TC_Organization'Access;
   TC_Organization_Contacts : aliased constant String := "organization_contacts";
   Ta_Organization_Contacts : constant Cst_String_Access := TC_Organization_Contacts'Access;
   TC_Recipient : aliased constant String := "recipient";
   Ta_Recipient : constant Cst_String_Access := TC_Recipient'Access;
   TC_Recipient_Kind : aliased constant String := "recipient_kind";
   Ta_Recipient_Kind : constant Cst_String_Access := TC_Recipient_Kind'Access;

   NC_Contact_Id : aliased constant String := "contact_id";
   N_Contact_Id : constant Cst_String_Access := NC_contact_id'Access;
   NC_Email_Address : aliased constant String := "email_address";
   N_Email_Address : constant Cst_String_Access := NC_email_address'Access;
   NC_Full_Name : aliased constant String := "full_name";
   N_Full_Name : constant Cst_String_Access := NC_full_name'Access;
   NC_Id : aliased constant String := "id";
   N_Id : constant Cst_String_Access := NC_id'Access;
   NC_Identifier : aliased constant String := "identifier";
   N_Identifier : constant Cst_String_Access := NC_identifier'Access;
   NC_Is_Human : aliased constant String := "is_human";
   N_Is_Human : constant Cst_String_Access := NC_is_human'Access;
   NC_Json : aliased constant String := "json";
   N_Json : constant Cst_String_Access := NC_json'Access;
   NC_Kind : aliased constant String := "kind";
   N_Kind : constant Cst_String_Access := NC_kind'Access;
   NC_Kind_Id : aliased constant String := "kind_id";
   N_Kind_Id : constant Cst_String_Access := NC_kind_id'Access;
   NC_Organization_Id : aliased constant String := "organization_id";
   N_Organization_Id : constant Cst_String_Access := NC_organization_id'Access;
   NC_Recipient_Id : aliased constant String := "recipient_id";
   N_Recipient_Id : constant Cst_String_Access := NC_recipient_id'Access;
end Database_Names;
