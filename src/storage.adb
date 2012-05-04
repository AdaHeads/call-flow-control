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
with Yolk.Log;

package body Storage is

   Null_Database_Connection_Pool : constant Database_Connection_Pool
     := (Hosts  => (others => null),
         Status => (others => Uninitialized));

   package Task_Association is new Ada.Task_Attributes
     (Database_Connection_Pool, Null_Database_Connection_Pool);

   DB_Descriptions               : constant array
     (Database_Connection_Priority) of
     Exec.Database_Description :=
       (Primary   => Postgres.Setup
            (Database      => Config.Get (DB_Name),
             User          => Config.Get (DB_User),
             Host          => Config.Get (DB_Host),
             Password      => Config.Get (DB_Password),
             SSL           => Postgres.Allow,
             Cache_Support => True),
        Secondary => Postgres.Setup
          (Database      => Config.Get (DB2_Name),
           User          => Config.Get (DB2_User),
           Host          => Config.Get (DB2_Host),
           Password      => Config.Get (DB2_Password),
           SSL           => Postgres.Allow,
           Cache_Support => True));

   --------------------------
   --  Get_DB_Connections  --
   --------------------------

   function Get_DB_Connections
     return Database_Connection_Pool
   is
      use GNATCOLL.SQL.Exec;
      use Yolk.Log;

      Connection_Pool : Database_Connection_Pool;
   begin
      Connection_Pool := Task_Association.Value;

      for k in Connection_Pool.Hosts'Range loop
         case Connection_Pool.Status (k) is
            when Uninitialized | Failed =>
               Trace (Handle  => Info,
                      Message => Database_Connection_Priority'Image (k) &
                      "-" &
                      Database_Connection_Status'Image
                        (Connection_Pool.Status (k)));

               Connection_Pool.Hosts (k) :=
                 DB_Descriptions (k).Build_Connection;

               if Check_Connection (Connection_Pool.Hosts (k)) then
                  Connection_Pool.Status (k) := Initialized;
               else
                  Connection_Pool.Status (k) := Failed;
               end if;
            when Initialized =>
               Trace (Handle  => Info,
                      Message => Database_Connection_Priority'Image (k) &
                      "-" &
                      Database_Connection_Status'Image
                        (Connection_Pool.Status (k)));

               Exec.Reset_Connection (Connection_Pool.Hosts (k),
                                      Database_Connection_Priority'Image (k));
         end case;
      end loop;

      Task_Association.Set_Value (Connection_Pool);

      return Connection_Pool;
   end Get_DB_Connections;

   ------------------------------------
   --  Register_Failed_DB_Connection --
   ------------------------------------

   procedure Register_Failed_DB_Connection
     (Pool : in Database_Connection_Pool)
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
