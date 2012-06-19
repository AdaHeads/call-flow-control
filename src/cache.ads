-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                  Cache                                    --
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

with Common;
with My_Configuration;
with Yolk.Cache.String_Keys;

package Cache is

   package My renames My_Configuration;

   package Contact_Cache is new Yolk.Cache.String_Keys
     (Element_Type      => Common.JSON_Small.Bounded_String,
      Cleanup_Size      => My.Config.Get (My.Cache_Size_Contact) + 1,
      Cleanup_On_Write  => True,
      Max_Element_Age   => My.Config.Get (My.Cache_Max_Element_Age),
      Reserved_Capacity => My.Config.Get (My.Cache_Size_Contact));
   --  Cache for individual contact JSON objects.

   package Contact_Full_Cache is new Yolk.Cache.String_Keys
     (Element_Type      => Common.JSON_Small.Bounded_String,
      Cleanup_Size      => My.Config.Get (My.Cache_Size_Contact) + 1,
      Cleanup_On_Write  => True,
      Max_Element_Age   => My.Config.Get (My.Cache_Max_Element_Age),
      Reserved_Capacity => My.Config.Get (My.Cache_Size_Contact));
   --  Cache for individual contact JSON objects. The contact JSON document
   --  SHOULD be complete with attributes.

   package Contact_Attributes_Cache is new Yolk.Cache.String_Keys
     (Element_Type      => Common.JSON_Small.Bounded_String,
      Cleanup_Size      => My.Config.Get (My.Cache_Size_Contact) + 1,
      Cleanup_On_Write  => True,
      Max_Element_Age   => My.Config.Get (My.Cache_Max_Element_Age),
      Reserved_Capacity => My.Config.Get (My.Cache_Size_Contact));
   --  Cache for individual contact attributes JSON objects.

   package Org_Contacts_Cache is new Yolk.Cache.String_Keys
     (Element_Type      => Common.JSON_Large.Bounded_String,
      Cleanup_Size      => My.Config.Get (My.Cache_Size_Organization) + 1,
      Cleanup_On_Write  => True,
      Max_Element_Age   => My.Config.Get (My.Cache_Max_Element_Age),
      Reserved_Capacity => My.Config.Get (My.Cache_Size_Organization));
   --  Cache for groups of contact JSON objects. The groups SHOULD be based on
   --  the organization the contacts belong to.

   package Org_Contacts_Full_Cache is new Yolk.Cache.String_Keys
     (Element_Type      => Common.JSON_Large.Bounded_String,
      Cleanup_Size      => My.Config.Get (My.Cache_Size_Organization) + 1,
      Cleanup_On_Write  => True,
      Max_Element_Age   => My.Config.Get (My.Cache_Max_Element_Age),
      Reserved_Capacity => My.Config.Get (My.Cache_Size_Organization));
   --  Cache for groups of contact JSON objects. The groups SHOULD be based on
   --  the organization the contacts belong to and the contact JSON document
   --  SHOULD be complete with attributes.

   package Org_Contacts_Attributes_Cache is new Yolk.Cache.String_Keys
     (Element_Type      => Common.JSON_Large.Bounded_String,
      Cleanup_Size      => My.Config.Get (My.Cache_Size_Organization) + 1,
      Cleanup_On_Write  => True,
      Max_Element_Age   => My.Config.Get (My.Cache_Max_Element_Age),
      Reserved_Capacity => My.Config.Get (My.Cache_Size_Organization));
   --  Cache for groups of contact attributes JSON objects. The groups SHOULD
   --  be based on the organization the contacts belong to.

   package Organization_Cache is new Yolk.Cache.String_Keys
     (Element_Type      => Common.JSON_Small.Bounded_String,
      Cleanup_Size      => My.Config.Get (My.Cache_Size_Organization) + 1,
      Cleanup_On_Write  => True,
      Max_Element_Age   => My.Config.Get (My.Cache_Max_Element_Age),
      Reserved_Capacity => My.Config.Get (My.Cache_Size_Organization));
   --  Cache for individual organization JSON objects.

end Cache;
