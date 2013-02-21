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

with Ada.Strings.Fixed;

with GNATCOLL.SQL.Postgres;

with Alice_Configuration;

package Storage.PostgreSQL is

   use Ada.Strings;
   use Alice_Configuration;

   function Port
     (Port : in Natural)
      return String is
     (" port=" & Fixed.Trim (Natural'Image (Port), Both));
   --  Construct the port string.

   Description : constant GNATCOLL.SQL.Exec.Database_Description
     := GNATCOLL.SQL.Postgres.Setup
       (Database => Config.Get (DB_Name),
        User     => Config.Get (DB_User),
        Host     => Config.Get (DB_Host) & Port (Config.Get (DB_Port)),
        Password => Config.Get (DB_Password),
        SSL      => GNATCOLL.SQL.Postgres.Allow);

end Storage.PostgreSQL;
