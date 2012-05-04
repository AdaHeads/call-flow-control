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

   type Database_Connection_Priority is (Primary, Secondary);

   type Database_Connection_Status is (Uninitialized, Initialized, Failed);

   type Database_Connection_Array is array
     (Database_Connection_Priority) of Exec.Database_Connection;

   type Database_Connection_Status_Array is array
     (Database_Connection_Priority) of Database_Connection_Status;

   type Database_Connection_Pool is
      record
         Hosts  : Database_Connection_Array := (others => null);
         Status : Database_Connection_Status_Array :=
                    (others => Uninitialized);
      end record;

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

   function Get_DB_Connections
     return Database_Connection_Pool;
   --  TODO

   procedure Register_Failed_DB_Connection
     (Pool : in Database_Connection_Pool);
   --  TODO

   function Trim
     (Source : in String)
      return String;
   --  Trim Source string on both sides.

end Storage;
