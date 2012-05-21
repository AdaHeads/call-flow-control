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

   type Database_Connection_Type is (Primary, Secondary);
   --  The Primary connection is READ/WRITE while the Secondary is READ, so for
   --  SELECT queries both can be used, whereas INSERT/UPDATE/DELETE will only
   --  work with the Primary connection.

   type Database_Connection_State is (Uninitialized, Initialized, Failed);
   --  The state of a database connection.
   --    Uninitialized : The connection has never been used.
   --    Initialized   : The connection has been connected to the database.
   --    Failed        : The connection failed.

   type Database_Connection is
      record
         Host  : Exec.Database_Connection;
         State : Database_Connection_State;
      end record;

   Null_Database_Connection : constant Database_Connection
     := (null, Uninitialized);

   type Database_Connection_Pool is array (Database_Connection_Type) of
     Database_Connection;

   package Contact_Cache is new Yolk.Cache.String_Keys
     (Element_Type      => Unbounded_String,
      Cleanup_Size      => Config.Get (Cache_Size_Contact) + 1,
      Cleanup_On_Write  => True,
      Max_Element_Age   => Config.Get (Cache_Max_Element_Age),
      Reserved_Capacity => Config.Get (Cache_Size_Contact));
   --  Cache for individual contact JSON objects.

   package Contact_Full_Cache is new Yolk.Cache.String_Keys
     (Element_Type      => Unbounded_String,
      Cleanup_Size      => Config.Get (Cache_Size_Contact) + 1,
      Cleanup_On_Write  => True,
      Max_Element_Age   => Config.Get (Cache_Max_Element_Age),
      Reserved_Capacity => Config.Get (Cache_Size_Contact));
   --  Cache for individual contact JSON objects. The contact JSON document
   --  SHOULD be complete with attributes.

   package Contact_Attributes_Cache is new Yolk.Cache.String_Keys
     (Element_Type      => Unbounded_String,
      Cleanup_Size      => Config.Get (Cache_Size_Contact) + 1,
      Cleanup_On_Write  => True,
      Max_Element_Age   => Config.Get (Cache_Max_Element_Age),
      Reserved_Capacity => Config.Get (Cache_Size_Contact));
   --  Cache for individual contact attributes JSON objects.

   package Org_Contacts_Cache is new Yolk.Cache.String_Keys
     (Element_Type      => Unbounded_String,
      Cleanup_Size      => Config.Get (Cache_Size_Organization) + 1,
      Cleanup_On_Write  => True,
      Max_Element_Age   => Config.Get (Cache_Max_Element_Age),
      Reserved_Capacity => Config.Get (Cache_Size_Organization));
   --  Cache for groups of contact JSON objects. The groups SHOULD be based on
   --  the organization the contacts belong to.

   package Org_Contacts_Full_Cache is new Yolk.Cache.String_Keys
     (Element_Type      => Unbounded_String,
      Cleanup_Size      => Config.Get (Cache_Size_Organization) + 1,
      Cleanup_On_Write  => True,
      Max_Element_Age   => Config.Get (Cache_Max_Element_Age),
      Reserved_Capacity => Config.Get (Cache_Size_Organization));
   --  Cache for groups of contact JSON objects. The groups SHOULD be based on
   --  the organization the contacts belong to and the contact JSON document
   --  SHOULD be complete with attributes.

   package Org_Contacts_Attributes_Cache is new Yolk.Cache.String_Keys
     (Element_Type      => Unbounded_String,
      Cleanup_Size      => Config.Get (Cache_Size_Organization) + 1,
      Cleanup_On_Write  => True,
      Max_Element_Age   => Config.Get (Cache_Max_Element_Age),
      Reserved_Capacity => Config.Get (Cache_Size_Organization));
   --  Cache for groups of contact attributes JSON objects. The groups SHOULD
   --  be based on the organization the contacts belong to.

   package Organization_Cache is new Yolk.Cache.String_Keys
     (Element_Type      => Unbounded_String,
      Cleanup_Size      => Config.Get (Cache_Size_Organization) + 1,
      Cleanup_On_Write  => True,
      Max_Element_Age   => Config.Get (Cache_Max_Element_Age),
      Reserved_Capacity => Config.Get (Cache_Size_Organization));
   --  Cache for individual organization JSON objects.

   function Get_DB_Connections
     return Database_Connection_Pool;
   --  Return an array with the primary and secondary database connections.
   --
   --  IMPORTANT:
   --  Only the primary connection is read/write. The secondary is read only,
   --  so be sure never to use the secondary connection for any insert/delete/
   --  update queries.

   procedure Register_Failed_DB_Connection
     (Pool : in Database_Connection_Pool);
   --  If a specific connection fails, set it to Storage.Failed and register
   --  the Database_Connection_Pool object as failed.
   --
   --  NOTE:
   --  A failed database connection is re-tried on every hit to the database,
   --  so it will be re-initialized as soon as the database host is back online
   --  again.

   function Trim
     (Source : in String)
      return String;
   --  Trim Source string on both sides.

end Storage;
