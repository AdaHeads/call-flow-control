-------------------------------------------------------------------------------
--                                                                           --
--                     Copyright (C) 2013-, AdaHeads K/S                     --
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

with Ada.Strings.Fixed;

with GNATCOLL.SQL.Postgres;

with Alice_Configuration;

separate (Storage.Connections)
function Build_Connection (As : Connected_Mode) return Instance is
   function Port_String (Port : in Natural) return String;
   function Port_String (Port : in Natural) return String is
      use Ada.Strings;
   begin
      return " port=" & Fixed.Trim (Natural'Image (Port), Both);
   end Port_String;

   use Alice_Configuration;

   Primary_Server : constant GNATCOLL.SQL.Exec.Database_Description :=
     GNATCOLL.SQL.Postgres.Setup
            (Database      => Config.Get (DB_Name),
             User          => Config.Get (DB_User),
             Host          => Config.Get (DB_Host) &
                                Port_String (Config.Get (DB_Port)),
             Password      => Config.Get (DB_Password),
             SSL           => GNATCOLL.SQL.Postgres.Allow,
             Cache_Support => True);

   Secondary_Server : constant GNATCOLL.SQL.Exec.Database_Description :=
     GNATCOLL.SQL.Postgres.Setup
          (Database      => Config.Get (DB2_Name),
           User          => Config.Get (DB2_User),
           Host          => Config.Get (DB2_Host) &
                              Port_String (Config.Get (DB2_Port)),
           Password      => Config.Get (DB2_Password),
           SSL           => GNATCOLL.SQL.Postgres.Allow,
           Cache_Support => True);
begin
   case As is
      when Read_Only =>
         return (State      => Read_Only,
                 Connection => Secondary_Server.Build_Connection);
      when Read_Write =>
         return (State      => Read_Write,
                 Connection => Primary_Server.Build_Connection);
   end case;
end Build_Connection;
