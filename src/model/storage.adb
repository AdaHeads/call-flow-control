-------------------------------------------------------------------------------
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
with Ada.Strings.Unbounded;
with Ada.Task_Attributes;

with Common;
with Storage.Connections;
with System_Message.Critical;

package body Storage is

   Database_Error : exception;

   package Task_DB_Connection is new Ada.Task_Attributes
     (Connections.Instance, Connections.Off_Line_Instance);
   --  Associates a specific task ID with a database connection instance.

   function Get_Connection
     (As : Connections.Connected_Mode)
      return Connections.Instance;
   --  Return a database connection instance. You will get a Read_Write
   --  connection as long as that is possible (ie. the primary server is up)
   --  even if you request a Read_Only connection. You will only get a
   --  Read_Only connection if the primary database server is down.
   --  If both the primary and secondary database servers are down, then you
   --  will get a null Instance.

   function Trim
     (Source : in String)
      return String;
   --  Trim Source string on both sides. This will clear away \n also. This
   --  function is here because the errors thrown by PostgreSQL is postfixed
   --  with a \n which we must remove before sending the message to syslogd.

   ----------------------
   --  Get_Connection  --
   ----------------------

   function Get_Connection
     (As : in Connections.Connected_Mode)
     return Connections.Instance
   is
      use Connections;

      Connection : Instance := Task_DB_Connection.Value;
   begin
      if Connection = Off_Line_Instance then
         Connection := Get (As => As);
         Task_DB_Connection.Set_Value (Connection);
      end if;

      return Connection;
   end Get_Connection;

   ----------------------------
   --  Process_Select_Query  --
   ----------------------------

   procedure Process_Select_Query
     (Process_Element    : not null access procedure (E : in Element);
      Prepared_Statement : in GNATCOLL.SQL.Exec.Prepared_Statement;
      Query_Parameters   : in GNATCOLL.SQL.Exec.SQL_Parameters)
   is
      use Ada.Strings.Unbounded;
      use Common;
      use Connections;
      use GNATCOLL.SQL;
      use GNATCOLL.SQL.Exec;
      use System_Message;

      function Operation_Succeeded
        return Boolean;
      --  Excecute the query and process the elements.

      C             : Database_Cursor;
      DB            : Instance := Get_Connection (As => Read_Only);
      Error_Message : Unbounded_String := Null_Unbounded_String;

      ---------------------------
      --  Operation_Succeeded  --
      ---------------------------

      function Operation_Succeeded
        return Boolean
      is
      begin
         if DB = Off_Line_Instance then
            return False;
         end if;

         C.Fetch (DB.Connection,
                  Prepared_Statement,
                  Query_Parameters);

         if DB.Connection.Success then
            while C.Has_Row loop
               Process_Element (Cursor_To_Element (C));
               C.Next;
            end loop;

            return True;
         else
            return False;
         end if;
      end Operation_Succeeded;
   begin
      if not Operation_Succeeded then
         Connections.Queue_Failed (Connection => DB);
         Task_DB_Connection.Set_Value (DB);

         DB := Get_Connection (As => Read_Only);
         --  First try failed. Get a new connection and try again.

         if not Operation_Succeeded then
            --  Second try also failed. Log error and raise Database_Error.

            if DB = Off_Line_Instance  then
               Error_Message := U
                 ("database connection is Storage.Connections.Null_Instance");
            else
               Error_Message := U (Trim (Exec.Error (DB.Connection)));
            end if;
            --  We need to grab the error here, because the connection is
            --  reset when we register it as failed.

            Connections.Queue_Failed (Connection => DB);
            Task_DB_Connection.Set_Value (DB);

            Critical.Lost_Database_Connection
              (Message => To_String (Error_Message));
            raise Database_Error;
         end if;
      end if;
   end Process_Select_Query;

   ----------------------------------------
   --  Stop_Connection_Maintenance_Task  --
   ----------------------------------------

   procedure Stop_Connection_Maintenance_Task
   is
   begin
      Storage.Connections.Stop_Maintenance_Task;
   end Stop_Connection_Maintenance_Task;

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
