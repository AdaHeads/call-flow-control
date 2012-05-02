-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                 Storage                                   --
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

with Ada.Strings.Unbounded;
with GNATCOLL.SQL.Exec;
with My_Configuration;
with Yolk.Cache.String_Keys;

package Storage is

   use Ada.Strings.Unbounded;
   use GNATCOLL.SQL;
   use My_Configuration;

   Database_Error : exception;

   package Contact_Cache is new Yolk.Cache.String_Keys
     (Element_Type      => Unbounded_String,
      Cleanup_Size      => Config.Get (Cache_Size_Contact) + 1,
      Cleanup_On_Write  => True,
      Max_Element_Age   => Config.Get (Cache_Max_Element_Age),
      Reserved_Capacity => Config.Get (Cache_Size_Contact));
   --  Cache for individual contact JSON objects.

   package Contact_Attributes_Cache is new Yolk.Cache.String_Keys
     (Element_Type      => Unbounded_String,
      Cleanup_Size      => Config.Get (Cache_Size_Contact) + 1,
      Cleanup_On_Write  => True,
      Max_Element_Age   => Config.Get (Cache_Max_Element_Age),
      Reserved_Capacity => Config.Get (Cache_Size_Contact));
   --  Cache for individual contact attributes JSON objects.

   package Contact_Tags_Cache is new Yolk.Cache.String_Keys
     (Element_Type      => Unbounded_String,
      Cleanup_Size      => Config.Get (Cache_Size_Contact) + 1,
      Cleanup_On_Write  => True,
      Max_Element_Age   => Config.Get (Cache_Max_Element_Age),
      Reserved_Capacity => Config.Get (Cache_Size_Contact));
   --  Cache for individual contact tags JSON objects.

   package Contacts_Cache is new Yolk.Cache.String_Keys
     (Element_Type      => Unbounded_String,
      Cleanup_Size      => Config.Get (Cache_Size_Organization) + 1,
      Cleanup_On_Write  => True,
      Max_Element_Age   => Config.Get (Cache_Max_Element_Age),
      Reserved_Capacity => Config.Get (Cache_Size_Organization));
   --  Cache for groups of contact JSON objects. The groups SHOULD be based on
   --  the organization the contacts belong to.

   package Contacts_Attributes_Cache is new Yolk.Cache.String_Keys
     (Element_Type      => Unbounded_String,
      Cleanup_Size      => Config.Get (Cache_Size_Organization) + 1,
      Cleanup_On_Write  => True,
      Max_Element_Age   => Config.Get (Cache_Max_Element_Age),
      Reserved_Capacity => Config.Get (Cache_Size_Organization));
   --  Cache for groups of contact attributes JSON objects. The groups SHOULD
   --  be based on the organization the contacts belong to.

   package Contacts_Tags_Cache is new Yolk.Cache.String_Keys
     (Element_Type      => Unbounded_String,
      Cleanup_Size      => Config.Get (Cache_Size_Organization) + 1,
      Cleanup_On_Write  => True,
      Max_Element_Age   => Config.Get (Cache_Max_Element_Age),
      Reserved_Capacity => Config.Get (Cache_Size_Organization));
   --  Cache for groups of contact tags JSON objects. The groups SHOULD be
   --  based on the organization the contacts belong to.

   package Organization_Cache is new Yolk.Cache.String_Keys
     (Element_Type      => Unbounded_String,
      Cleanup_Size      => Config.Get (Cache_Size_Organization) + 1,
      Cleanup_On_Write  => True,
      Max_Element_Age   => Config.Get (Cache_Max_Element_Age),
      Reserved_Capacity => Config.Get (Cache_Size_Organization));
   --  Cache for individual organization JSON objects.

   function Get_DB_Connection
     return Exec.Database_Connection;
   --  Return a connection to the database.
   --
   --  IMPORTANT!
   --  You should _NEVER_ use this connection for any write operations (delete,
   --  update, insert) because if the primary database server is offline for
   --  some reason, a READ ONLY connection to the backup server is returned
   --  instead.

end Storage;
