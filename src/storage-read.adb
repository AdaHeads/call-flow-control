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

with Cache;
with Errors;
with JSONIFY;
with Storage.Queries;

package body Storage.Read is

   procedure Failed_Query
     (Connection_Pool : in out DB_Conn_Pool;
      Connection_Type : in     DB_Conn_Type);
   --  If a query fails:
   --    1. Set the connection state to Failed.
   --    2. Raise the Database_Error exception if
   --         Connection_Type = Database_Connection_Type'Last
   --    3. If 2 is not True, Output a message to the Error log trace.

   --------------------
   --  Failed_Query  --
   --------------------

   procedure Failed_Query
     (Connection_Pool : in out DB_Conn_Pool;
      Connection_Type : in     DB_Conn_Type)
   is
      use Errors;
      use GNATCOLL.SQL;

      Trimmed_DB_Error : constant String
        := Trim (Exec.Error (Connection_Pool (Connection_Type).Host));

      Connection       : constant String
        := DB_Conn_Type'Image (Connection_Type);

      Message : constant String :=  Trimmed_DB_Error &  "-" & Connection;
   begin
      Connection_Pool (Connection_Type).State := Failed;
      Register_Failed_DB_Connection (Pool => Connection_Pool);

      if Connection_Type = DB_Conn_Type'Last then
         raise Database_Error with Message;
      else
         Error_Handler (Message);
      end if;
   end Failed_Query;

   -------------------
   --  Get_Contact  --
   -------------------

   function Get_Contact
     (Ce_Id : in String)
      return Common.JSON_Small.Bounded_String
   is
      use Cache;
      use Common;
      use Errors;
      use GNATCOLL.SQL.Exec;

      C              : Queries.Contact_Cursor;
      DB_Connections : DB_Conn_Pool := Get_DB_Connections;
      Value          : JSON_Small.Bounded_String;
   begin
      Fetch_Data :
      for k in DB_Connections'Range loop
         C.Fetch (DB_Connections (k).Host,
                  Queries.Contact_Query,
                  Params => (1 => +Natural'Value (Ce_Id)));

         if DB_Connections (k).Host.Success then
            JSONIFY.Contact (C, Value);

            if C.Processed_Rows > 0 then
               Contact_Cache.Write (Key   => Ce_Id,
                                    Value => Value);
            end if;

            exit Fetch_Data;
         else
            Failed_Query (Connection_Pool => DB_Connections,
                          Connection_Type => k);
         end if;
      end loop Fetch_Data;

      return Value;
   end Get_Contact;

   ------------------------------
   --  Get_Contact_Attributes  --
   ------------------------------

   function Get_Contact_Attributes
     (Ce_Id : in String)
      return Common.JSON_Small.Bounded_String
   is
      use Cache;
      use Common;
      use Errors;
      use GNATCOLL.SQL.Exec;

      C              : Queries.Contact_Attributes_Cursor;
      DB_Connections : DB_Conn_Pool := Get_DB_Connections;
      Value          : JSON_Small.Bounded_String;
   begin
      Fetch_Data :
      for k in DB_Connections'Range loop
         C.Fetch (DB_Connections (k).Host,
                  Queries.Contact_Attributes_Query,
                  Params => (1 => +Natural'Value (Ce_Id)));

         if DB_Connections (k).Host.Success then
            JSONIFY.Contact_Attributes (C, Value);

            if C.Processed_Rows > 0 then
               Contact_Attributes_Cache.Write (Key   => Ce_Id,
                                               Value => Value);
            end if;

            exit Fetch_Data;
         else
            Failed_Query (Connection_Pool => DB_Connections,
                          Connection_Type => k);
         end if;
      end loop Fetch_Data;

      return Value;
   end Get_Contact_Attributes;

   ------------------------
   --  Get_Contact_Full  --
   ------------------------

   function Get_Contact_Full
     (Ce_Id : in String)
      return Common.JSON_Small.Bounded_String
   is
      use Cache;
      use Common;
      use Errors;
      use GNATCOLL.SQL.Exec;

      Cursor         : Queries.Contact_Full_Cursor;
      DB_Connections : DB_Conn_Pool := Get_DB_Connections;
      Value          : JSON_Small.Bounded_String;
   begin
      Fetch_Data :
      for k in DB_Connections'Range loop
         Cursor.Fetch (DB_Connections (k).Host,
                       Queries.Contact_Full_Query,
                       Params => (1 => +Natural'Value (Ce_Id)));

         if DB_Connections (k).Host.Success then
            JSONIFY.Contact_Full (Cursor, Value);

            if Cursor.Processed_Rows > 0 then
               Contact_Full_Cache.Write (Key   => Ce_Id,
                                      Value => Value);
            end if;

            exit Fetch_Data;
         else
            Failed_Query (Connection_Pool => DB_Connections,
                          Connection_Type => k);
         end if;
      end loop Fetch_Data;

      return Value;
   end Get_Contact_Full;

   ------------------------
   --  Get_Org_Contacts  --
   ------------------------

   function Get_Org_Contacts
     (Org_Id : in String)
      return Common.JSON_Large.Bounded_String
   is
      use Cache;
      use Common;
      use Errors;
      use GNATCOLL.SQL.Exec;

      Cursor         : Queries.Org_Contacts_Cursor;
      DB_Connections : DB_Conn_Pool := Get_DB_Connections;
      Value          : JSON_Large.Bounded_String;
   begin
      Fetch_Data :
      for k in DB_Connections'Range loop
         Cursor.Fetch (DB_Connections (k).Host,
                       Queries.Org_Contacts_Query,
                       Params => (1 => +Natural'Value (Org_Id)));

         if DB_Connections (k).Host.Success then
            JSONIFY.Org_Contacts (Cursor, Value);

            if Cursor.Processed_Rows > 0 then
               Org_Contacts_Cache.Write (Key   => Org_Id,
                                         Value => Value);
            end if;

            exit Fetch_Data;
         else
            Failed_Query (Connection_Pool => DB_Connections,
                          Connection_Type => k);
         end if;
      end loop Fetch_Data;

      return Value;
   end Get_Org_Contacts;

   -----------------------------------
   --  Get_Org_Contacts_Attributes  --
   -----------------------------------

   function Get_Org_Contacts_Attributes
     (Org_Id : in String)
      return Common.JSON_Large.Bounded_String
   is
      use Cache;
      use Common;
      use Errors;
      use GNATCOLL.SQL.Exec;

      Cursor         : Queries.Org_Contacts_Attributes_Cursor;
      DB_Connections : DB_Conn_Pool := Get_DB_Connections;
      Value          : JSON_Large.Bounded_String;
   begin
      Fetch_Data :
      for k in DB_Connections'Range loop
         Cursor.Fetch (DB_Connections (k).Host,
                       Queries.Org_Contacts_Attributes_Query,
                       Params => (1 => +Natural'Value (Org_Id)));

         if DB_Connections (k).Host.Success then
            JSONIFY.Org_Contacts_Attributes (Cursor, Value);

            if Cursor.Processed_Rows > 0 then
               Org_Contacts_Attributes_Cache.Write (Key   => Org_Id,
                                                    Value => Value);
            end if;

            exit Fetch_Data;
         else
            Failed_Query (Connection_Pool => DB_Connections,
                          Connection_Type => k);
         end if;
      end loop Fetch_Data;

      return Value;
   end Get_Org_Contacts_Attributes;

   -----------------------------
   --  Get_Org_Contacts_Full  --
   -----------------------------

   function Get_Org_Contacts_Full
     (Org_Id : in String)
      return Common.JSON_Large.Bounded_String
   is
      use Cache;
      use Common;
      use Errors;
      use GNATCOLL.SQL.Exec;

      Cursor         : Queries.Org_Contacts_Full_Cursor;
      DB_Connections : DB_Conn_Pool := Get_DB_Connections;
      Value          : JSON_Large.Bounded_String;
   begin
      Fetch_Data :
      for k in DB_Connections'Range loop
         Cursor.Fetch (DB_Connections (k).Host,
                       Queries.Org_Contacts_Full_Query,
                       Params => (1 => +Natural'Value (Org_Id)));

         if DB_Connections (k).Host.Success then
            JSONIFY.Org_Contacts_Full (Cursor, Value);

            if Cursor.Processed_Rows > 0 then
               Org_Contacts_Full_Cache.Write (Key   => Org_Id,
                                              Value => Value);
            end if;

            exit Fetch_Data;
         else
            Failed_Query (Connection_Pool => DB_Connections,
                          Connection_Type => k);
         end if;
      end loop Fetch_Data;

      return Value;
   end Get_Org_Contacts_Full;

   ------------------------
   --  Get_Organization  --
   ------------------------

   function Get_Organization
     (Org_Id : in String)
      return Common.JSON_Small.Bounded_String
   is
      use Cache;
      use Common;
      use Errors;
      use GNATCOLL.SQL.Exec;

      Cursor         : Queries.Organization_Cursor;
      DB_Connections : DB_Conn_Pool := Get_DB_Connections;
      Value          : JSON_Small.Bounded_String :=
                         JSON_Small.Null_Bounded_String;
   begin
      Fetch_Data :
      for k in DB_Connections'Range loop
         Cursor.Fetch (DB_Connections (k).Host,
                       Queries.Organization_Query,
                       Params => (1 => +Natural'Value (Org_Id)));

         if DB_Connections (k).Host.Success then
            JSONIFY.Organization (Cursor, Value);

            if Cursor.Processed_Rows > 0 then
               Organization_Cache.Write (Key   => Org_Id,
                                         Value => Value);
            end if;

            exit Fetch_Data;
         else
            Failed_Query (Connection_Pool => DB_Connections,
                          Connection_Type => k);
         end if;
      end loop Fetch_Data;

      return Value;
   end Get_Organization;

end Storage.Read;
