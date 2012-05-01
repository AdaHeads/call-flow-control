-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                               Storage.Read                                --
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

package Storage.Read is

   function Contact
     (Ce_Id  : in Natural)
      return String;
   --  Return a Contact JSON string.

   function Contact_Attributes
     (Ce_Id  : in Natural)
      return String;
   --  Return a Contact_Attributes JSON string.

   function Contacts
     (Org_Id : in Natural)
      return String;
   --  Return a Contacts JSON string.

   function Contacts_Attributes
     (Org_Id  : in Natural)
      return String;
   --  Return a Contacts_Attributes JSON string.

   function Organization
     (Org_Id : in Natural)
      return String;
   --  Return an Organization JSON string.

   function Organization_Attributes
     (Org_Id : in Natural)
      return String;
   --  Return an Organization_Attributes JSON string.

end Storage.Read;
