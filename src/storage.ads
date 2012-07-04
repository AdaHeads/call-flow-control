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

with AWS.Status;
with AWS.Messages;
with Common;
with GNATCOLL.SQL.Exec;

package Storage is

   type DB_Conn_Type is (Primary, Secondary);
   --  The Primary connection is READ/WRITE while the Secondary is READ, so for
   --  SELECT queries both can be used, whereas INSERT/UPDATE/DELETE will only
   --  work with the Primary connection.

   type DB_Conn_State is (Uninitialized, Initialized, Failed);
   --  The state of a database connection.
   --    Uninitialized : The connection has never been used.
   --    Initialized   : The connection has been connected to the database.
   --    Failed        : The connection failed.

   type DB_Conn is
      record
         Host  : GNATCOLL.SQL.Exec.Database_Connection;
         State : DB_Conn_State;
      end record;

   Null_Database_Connection : constant DB_Conn := (null, Uninitialized);

   type DB_Conn_Pool is array (DB_Conn_Type) of DB_Conn;

   procedure Failed_Query
     (Connection_Pool : in out DB_Conn_Pool;
      Connection_Type : in     DB_Conn_Type);
   --  If a query fails:
   --    1. Set the connection state to Failed.
   --    2. Raise the Database_Error exception if
   --         Connection_Type = Database_Connection_Type'Last
   --    3. If 2 is not True, Output a message to the Error log trace.

   function Get_DB_Connections
     return DB_Conn_Pool;
   --  Return an array with the primary and secondary database connections.
   --
   --  IMPORTANT:
   --  Only the primary connection is read/write. The secondary is read only,
   --  so be sure never to use the secondary connection for any insert/delete/
   --  update queries.

   procedure Register_Failed_DB_Connection
     (Pool : in DB_Conn_Pool);
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
   --  Trim Source string on both sides. This will clear away \n also. This
   --  function is here because the errors thrown by PostgreSQL is postfixed
   --  with a \n which we must remove before sending the message to syslogd.

   generic

      type Cursor is new GNATCOLL.SQL.Exec.Forward_Cursor with private;

      with function Query
        return GNATCOLL.SQL.Exec.Prepared_Statement;
      --  TODO: Write comment

      with procedure JSONIFY
        (C     : in out Cursor;
         Value : in out Common.JSON_String);
      --  TODO: Write comment

      with function Query_Parameters
        (Request : in AWS.Status.Data)
         return GNATCOLL.SQL.Exec.SQL_Parameters;
      --  TODO: Write comment

      with procedure Write_To_Cache
        (Key   : in String;
         Value : in Common.JSON_String);
      --  TODO: Write comment

   package Generic_Query_To_JSON is

      procedure Generate
        (Key     : in      String;
         Request : in      AWS.Status.Data;
         Status  :     out AWS.Messages.Status_Code;
         Value   :     out Common.JSON_String);
      --  TODO: Write comment

   end Generic_Query_To_JSON;

end Storage;
