-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                 Storage                                   --
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

with Ada.Characters.Latin_1;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Task_Attributes;
with GNATCOLL.SQL.Postgres;
with My_Configuration;
with System_Message.Critical;

package body Storage is

   use My_Configuration;

   Database_Error : exception;

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

   Null_Pool : constant DB_Conn_Pool :=
                 (others => Null_Database_Connection);

   package Task_Association is new Ada.Task_Attributes
     (DB_Conn_Pool, Null_Pool);
   --  Associates a specific task ID with a Database_Connection_Pool.

   function DB_Port_String
     (Port : in Natural)
      return String;
   --  Return the PostgreSQL port=n part of the connection string.

   procedure Failed_Query
     (Connection_Pool : in out DB_Conn_Pool;
      Connection_Type : in     DB_Conn_Type);
   --  If a query fails:
   --    1. Set the connection state to Failed.
   --    2. Raise the Database_Error exception if
   --         Connection_Type = Database_Connection_Type'Last

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

   ----------------------
   --  DB_Port_String  --
   ----------------------

   function DB_Port_String
     (Port : in Natural)
      return String
   is
      use Ada.Strings;
   begin
      return " port=" & Fixed.Trim (Natural'Image (Port), Both);
   end DB_Port_String;

   DB_Descriptions : constant array (DB_Conn_Type) of
     GNATCOLL.SQL.Exec.Database_Description :=
       (Primary   => GNATCOLL.SQL.Postgres.Setup
            (Database      => Config.Get (DB_Name),
             User          => Config.Get (DB_User),
             Host          => Config.Get (DB_Host) &
                                DB_Port_String (Config.Get (DB_Port)),
             Password      => Config.Get (DB_Password),
             SSL           => GNATCOLL.SQL.Postgres.Allow,
             Cache_Support => True),
        Secondary => GNATCOLL.SQL.Postgres.Setup
          (Database      => Config.Get (DB2_Name),
           User          => Config.Get (DB2_User),
           Host          => Config.Get (DB2_Host) &
                              DB_Port_String (Config.Get (DB2_Port)),
           Password      => Config.Get (DB2_Password),
           SSL           => GNATCOLL.SQL.Postgres.Allow,
           Cache_Support => True));

   --------------------
   --  Failed_Query  --
   --------------------

   procedure Failed_Query
     (Connection_Pool : in out DB_Conn_Pool;
      Connection_Type : in     DB_Conn_Type)
   is
      use GNATCOLL.SQL;
      use System_Message;

      Trimmed_DB_Error : constant String
        := Trim (Exec.Error (Connection_Pool (Connection_Type).Host));
   begin
      Connection_Pool (Connection_Type).State := Failed;
      Register_Failed_DB_Connection (Pool => Connection_Pool);

      if Connection_Type = DB_Conn_Type'Last then
         raise Database_Error with Trimmed_DB_Error;
      else
         Critical.Lost_Database_Connection
           (Message => Trimmed_DB_Error & " PRIMARY");
      end if;
   end Failed_Query;

   --------------------------
   --  Get_DB_Connections  --
   --------------------------

   function Get_DB_Connections
     return DB_Conn_Pool
   is
      use GNATCOLL.SQL.Exec;

      Connection_Pool : DB_Conn_Pool := Task_Association.Value;
   begin
      for k in Connection_Pool'Range loop
         case Connection_Pool (k).State is
            when Uninitialized | Failed =>
               Connection_Pool (k).Host :=
                 DB_Descriptions (k).Build_Connection;

               if Check_Connection (Connection_Pool (k).Host) then
                  Connection_Pool (k).State := Initialized;
               else
                  Connection_Pool (k).State := Failed;
               end if;
            when Initialized =>
               Reset_Connection (Connection_Pool (k).Host,
                                 DB_Conn_Type'Image (k));
         end case;
      end loop;

      Task_Association.Set_Value (Connection_Pool);

      return Connection_Pool;
   end Get_DB_Connections;

   ---------------------
   --  Process_Query  --
   ---------------------

   procedure Process_Query
     (Process_Element    : not null access procedure (E : in Element);
      Prepared_Statement : in GNATCOLL.SQL.Exec.Prepared_Statement;
      Query_Parameters   : in GNATCOLL.SQL.Exec.SQL_Parameters)
   is
      use Ada.Exceptions;
      use GNATCOLL.SQL.Exec;
      use Storage;
      use System_Message;

      C              : Database_Cursor;
      DB_Connections : DB_Conn_Pool := Get_DB_Connections;
   begin
      Fetch_Data :
      for K in DB_Connections'Range loop
         C.Fetch (DB_Connections (K).Host,
                  Prepared_Statement,
                  Query_Parameters);

         if DB_Connections (K).Host.Success then
            while C.Has_Row loop
               Process_Element (Cursor_To_Element (C));
               C.Next;
            end loop;

            exit Fetch_Data;
         else
            Storage.Failed_Query (Connection_Pool => DB_Connections,
                                  Connection_Type => K);
         end if;
      end loop Fetch_Data;
   exception
      when Event : Database_Error =>
         Critical.Lost_Database_Connection
           (Message => Exception_Message (Event) & " SECONDARY");
         raise Database_Error;
   end Process_Query;

   ------------------------------------
   --  Register_Failed_DB_Connection --
   ------------------------------------

   procedure Register_Failed_DB_Connection
     (Pool : in DB_Conn_Pool)
   is
   begin
      Task_Association.Set_Value (Pool);
   end Register_Failed_DB_Connection;

   ------------
   --  Trim  --
   ------------

   function Trim
     (Source : in String)
      return String
   is
      use Ada.Strings;
   begin
      if Source (Source'Last) = Ada.Characters.Latin_1.LF then
         return Source (Source'First ..  Source'Last - 1);
      end if;

      return Fixed.Trim (Source, Both);
   end Trim;

end Storage;
