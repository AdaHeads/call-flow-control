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

   ---------------
   --  Contact  --
   ---------------

   procedure Contact
     (Ce_Id       : in     String;
      Status_Code :    out AWS.Messages.Status_Code;
      Value       :    out Common.JSON_Small.Bounded_String)
   is
      use Cache;
      use GNATCOLL.SQL.Exec;

      C              : Queries.Contact_Cursor;
      DB_Connections : DB_Conn_Pool := Get_DB_Connections;
   begin
      Status_Code := AWS.Messages.S500;

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

               Status_Code := AWS.Messages.S200;
            else
               Status_Code := AWS.Messages.S404;
            end if;

            exit Fetch_Data;
         else
            Failed_Query (Connection_Pool => DB_Connections,
                          Connection_Type => k);
         end if;
      end loop Fetch_Data;
   end Contact;

   --------------------------
   --  Contact_Attributes  --
   --------------------------

   procedure Contact_Attributes
     (Ce_Id       : in     String;
      Status_Code :    out AWS.Messages.Status_Code;
      Value       :    out Common.JSON_Small.Bounded_String)
   is
      use Cache;
      use GNATCOLL.SQL.Exec;

      C              : Queries.Contact_Attributes_Cursor;
      DB_Connections : DB_Conn_Pool := Get_DB_Connections;
   begin
      Status_Code := AWS.Messages.S500;

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

               Status_Code := AWS.Messages.S200;
            else
               Status_Code := AWS.Messages.S404;
            end if;

            exit Fetch_Data;
         else
            Failed_Query (Connection_Pool => DB_Connections,
                          Connection_Type => k);
         end if;
      end loop Fetch_Data;
   end Contact_Attributes;

   --------------------
   --  Contact_Full  --
   --------------------

   procedure Contact_Full
     (Ce_Id       : in     String;
      Status_Code :    out AWS.Messages.Status_Code;
      Value       :    out Common.JSON_Small.Bounded_String)
   is
      use Cache;
      use GNATCOLL.SQL.Exec;

      Cursor         : Queries.Contact_Full_Cursor;
      DB_Connections : DB_Conn_Pool := Get_DB_Connections;
   begin
      Status_Code := AWS.Messages.S500;

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

               Status_Code := AWS.Messages.S200;
            else
               Status_Code := AWS.Messages.S404;
            end if;

            exit Fetch_Data;
         else
            Failed_Query (Connection_Pool => DB_Connections,
                          Connection_Type => k);
         end if;
      end loop Fetch_Data;
   end Contact_Full;

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

   --------------------
   --  Org_Contacts  --
   --------------------

   procedure Org_Contacts
     (Org_Id      : in     String;
      Status_Code :    out AWS.Messages.Status_Code;
      Value       :    out Common.JSON_Large.Bounded_String)
   is
      use Cache;
      use GNATCOLL.SQL.Exec;

      Cursor         : Queries.Org_Contacts_Cursor;
      DB_Connections : DB_Conn_Pool := Get_DB_Connections;
   begin
      Status_Code := AWS.Messages.S500;

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

               Status_Code := AWS.Messages.S200;
            else
               Status_Code := AWS.Messages.S404;
            end if;

            exit Fetch_Data;
         else
            Failed_Query (Connection_Pool => DB_Connections,
                          Connection_Type => k);
         end if;
      end loop Fetch_Data;
   end Org_Contacts;

   -------------------------------
   --  Org_Contacts_Attributes  --
   -------------------------------

   procedure Org_Contacts_Attributes
     (Org_Id      : in     String;
      Status_Code :    out AWS.Messages.Status_Code;
      Value       :    out Common.JSON_Large.Bounded_String)
   is
      use Cache;
      use GNATCOLL.SQL.Exec;

      Cursor         : Queries.Org_Contacts_Attributes_Cursor;
      DB_Connections : DB_Conn_Pool := Get_DB_Connections;
   begin
      Status_Code := AWS.Messages.S500;

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

               Status_Code := AWS.Messages.S200;
            else
               Status_Code := AWS.Messages.S404;
            end if;

            exit Fetch_Data;
         else
            Failed_Query (Connection_Pool => DB_Connections,
                          Connection_Type => k);
         end if;
      end loop Fetch_Data;
   end Org_Contacts_Attributes;

   -------------------------
   --  Org_Contacts_Full  --
   -------------------------

   procedure Org_Contacts_Full
     (Org_Id      : in     String;
      Status_Code :    out AWS.Messages.Status_Code;
      Value       :    out Common.JSON_Large.Bounded_String)
   is
      use Cache;
      use GNATCOLL.SQL.Exec;

      Cursor         : Queries.Org_Contacts_Full_Cursor;
      DB_Connections : DB_Conn_Pool := Get_DB_Connections;
   begin
      Status_Code := AWS.Messages.S500;

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

               Status_Code := AWS.Messages.S200;
            else
               Status_Code := AWS.Messages.S404;
            end if;

            exit Fetch_Data;
         else
            Failed_Query (Connection_Pool => DB_Connections,
                          Connection_Type => k);
         end if;
      end loop Fetch_Data;
   end Org_Contacts_Full;

   --------------------
   --  Organization  --
   --------------------

   procedure Organization
     (Org_Id      : in     String;
      Status_Code :    out AWS.Messages.Status_Code;
      Value       :    out Common.JSON_Small.Bounded_String)
   is
      use Cache;
      use GNATCOLL.SQL.Exec;

      Cursor         : Queries.Organization_Cursor;
      DB_Connections : DB_Conn_Pool := Get_DB_Connections;
   begin
      Status_Code := AWS.Messages.S500;

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

               Status_Code := AWS.Messages.S200;
            else
               Status_Code := AWS.Messages.S404;
            end if;

            exit Fetch_Data;
         else
            Failed_Query (Connection_Pool => DB_Connections,
                          Connection_Type => k);
         end if;
      end loop Fetch_Data;
   end Organization;

end Storage.Read;
