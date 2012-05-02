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

with GNATCOLL.SQL.Postgres;
with Yolk.Log;

package body Storage is

   DB_Description : constant Exec.Database_Description :=
                      Postgres.Setup
                        (Database      => Config.Get (DB_Name),
                         User          => Config.Get (DB_User),
                         Host          => Config.Get (DB_Host),
                         Password      => Config.Get (DB_Password),
                         SSL           => Postgres.Allow,
                         Cache_Support => True);

   DB2_Description : constant Exec.Database_Description :=
                       Postgres.Setup
                         (Database      => Config.Get (DB2_Name),
                          User          => Config.Get (DB2_User),
                          Host          => Config.Get (DB2_Host),
                          Password      => Config.Get (DB2_Password),
                          SSL           => Postgres.Allow,
                          Cache_Support => True);

   -------------------------
   --  Get_DB_Connection  --
   -------------------------

   function Get_DB_Connection
     return Exec.Database_Connection
   is
      use Yolk.Log;

      C : Exec.Database_Connection;
   begin
      C := Exec.Get_Task_Connection (DB_Description);

      if Exec.Check_Connection (C) then
         Trace (Handle  => Info,
                Message => "SUCCESS WITH PRIMARY DB!");
         return C;
      else
         C := Exec.Get_Task_Connection (DB2_Description);
         if Exec.Check_Connection (C) then
            Trace (Handle  => Info,
                   Message => "SUCCESS WITH SECONDARY DB!");
         else
            Trace (Handle  => Info,
                   Message => "FAILURE WITH SECONDARY DB!");
         end if;

         return C;
      end if;
   end Get_DB_Connection;

end Storage;
