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

   function Get_Contact
     (Ce_Id  : in String)
      return String;
   --  Return a Contact JSON string. This contains the data for ONE contact-
   --  entity.

   function Get_Contact_Attributes
     (Ce_Id  : in String)
      return String;
   --  Return a Contact_Attributes JSON string. This contains attributes for
   --  ONE contactentity. Note that one contactentity can have several
   --  different attribute sets, depending on the organization the contact
   --  belongs to.

   function Get_Org_Contacts
     (Org_Id : in String)
      return String;
   --  Return a Contacts JSON string. This contains all the contactentities
   --  belonging to Org_Id.

   function Get_Org_Contacts_Attributes
     (Org_Id  : in String)
      return String;
   --  Return a Contacts_Attributes JSON string. This contains all the
   --  contactentity attributes that relates to the given Org_Id, meaning one
   --  set of attributes per contactentity that relates to Org_Id.

   function Get_Organization
     (Org_Id : in String)
      return String;
   --  Return an Organization JSON string. This contains the data for ONE
   --  organization.

end Storage.Read;
