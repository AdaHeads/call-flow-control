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
with Ada.Strings.Fixed;
with Ada.Task_Attributes;
with Errors;
with GNATCOLL.SQL.Postgres;
with HTTP_Codes;
with My_Configuration;

package body Storage is

   package My renames My_Configuration;

   Null_Pool : constant DB_Conn_Pool :=
                 (others => Null_Database_Connection);

   package Task_Association is new Ada.Task_Attributes
     (DB_Conn_Pool, Null_Pool);
   --  Associates a specific task ID with a Database_Connection_Pool.

   function DB_Port_String
     (Port : in Natural)
      return String;
   --  Return the PostgreSQL port=n part of the connection string.

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
            (Database      => My.Config.Get (My.DB_Name),
             User          => My.Config.Get (My.DB_User),
             Host          => My.Config.Get (My.DB_Host) &
                                DB_Port_String (My.Config.Get (My.DB_Port)),
             Password      => My.Config.Get (My.DB_Password),
             SSL           => GNATCOLL.SQL.Postgres.Allow,
             Cache_Support => True),
        Secondary => GNATCOLL.SQL.Postgres.Setup
          (Database      => My.Config.Get (My.DB2_Name),
           User          => My.Config.Get (My.DB2_User),
           Host          => My.Config.Get (My.DB2_Host) &
                              DB_Port_String (My.Config.Get (My.DB2_Port)),
           Password      => My.Config.Get (My.DB2_Password),
           SSL           => GNATCOLL.SQL.Postgres.Allow,
           Cache_Support => True));

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
   --  Generic_JSON  --
   --------------------

   package body Generic_Query_To_JSON is

      procedure Generate
        (Key              : in      String;
         Request          : in      AWS.Status.Data;
         Status           :     out AWS.Messages.Status_Code;
         Value            :     out Common.JSON_String)
      is
         use GNATCOLL.SQL.Exec;
         use HTTP_Codes;
         use Storage;

         C              : Cursor;
         DB_Connections : DB_Conn_Pool := Get_DB_Connections;
      begin
         Status := Server_Error;
         --  Assume the worst.

         Fetch_Data :
         for k in DB_Connections'Range loop
            C.Fetch (DB_Connections (k).Host,
                     Query,
                     Params => Query_Parameters (Request));

            if DB_Connections (k).Host.Success then
               JSONIFY (C, Value);

               if C.Processed_Rows > 0 then
                  Write_To_Cache (Key   => Key,
                                  Value => Value);

                  Status := OK;
               else
                  Status := Not_Found;
               end if;

               exit Fetch_Data;
            else
               Storage.Failed_Query (Connection_Pool => DB_Connections,
                                     Connection_Type => k);
            end if;
         end loop Fetch_Data;
      end Generate;

   end Generic_Query_To_JSON;

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
