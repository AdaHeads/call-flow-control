-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                               Storage.Read                                --
--                                                                           --
--                                  BODY                                     --
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

package body Storage.Read is

   ---------------
   --  Contact  --
   ---------------

   function Contact
     (Ce_Id  : in Natural)
      return String
   is
   begin
      return "{CONTACT ce_id:" & Natural'Image (Ce_Id) & "}";
   end Contact;

   --------------------------
   --  Contact_Attributes  --
   --------------------------

   function Contact_Attributes
     (Ce_Id  : in Natural)
      return String
   is
   begin
      return "{CONTACT_ATTRIBUTES ce_id:" & Natural'Image (Ce_Id) & "}";
   end Contact_Attributes;

   ----------------
   --  Contacts  --
   ----------------

   function Contacts
     (Org_Id : in Natural)
      return String
   is
   begin
      return "{CONTACTS org_id:" & Natural'Image (Org_Id) & "}";
   end Contacts;

   ---------------------------
   --  Contacts_Attributes  --
   ---------------------------

   function Contacts_Attributes
     (Org_Id : in Natural)
      return String
   is
   begin
      return "{CONTACTS_ATTRIBUTES org_id:" & Natural'Image (Org_Id) & "}";
   end Contacts_Attributes;

   --------------------
   --  Organization  --
   --------------------

   function Organization
     (Org_Id : in Natural)
      return String
   is
   begin
      return "{ORGANIZATION org_id:" & Natural'Image (Org_Id) & "}";
   end Organization;

   -------------------------------
   --  Organization_Attributes  --
   -------------------------------

   function Organization_Attributes
     (Org_Id : in Natural)
      return String
   is
   begin
      return "{ORGANIZATION_ATTRIBUTES org_id:" & Natural'Image (Org_Id) & "}";
   end Organization_Attributes;

end Storage.Read;
