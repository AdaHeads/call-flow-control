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
with GNATCOLL.SQL.Postgres;
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
