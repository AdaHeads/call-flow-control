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

with GNATCOLL.SQL.Exec;

private package Storage.Connections is

   type Mode is (Off_Line, Read_Only, Read_Write);
   --  Ordered by capability! Do not re-arrange these.

   subtype Connected_Mode is Mode range Read_Only .. Read_Write;

   type Instance (State : Mode := Off_Line) is
      record
         case State is
            when Off_Line =>
               null;
            when Read_Only | Read_Write =>
               Connection : not null GNATCOLL.SQL.Exec.Database_Connection;
         end case;
      end record;

   Off_Line_Instance : constant Instance := (State => Off_Line);

   function Get
     (As : in Connected_Mode)
      return Instance;
   --  Get a connection Instance. This will either be null or an actual
   --  GNATColl Database_Connection object. If it is null then it means the
   --  connection Maintenance task could not connect to the database defined
   --  by Mode (primary DB for Read_Write, secondary DB for Read_Only).

   procedure Queue_Failed
     (Connection : in out Instance);
   --  Put the Connection in the failed queue and set its state to Off_Line.

   procedure Stop_Maintenance_Task;
   --  Kill the connection maintenance task.

end Storage.Connections;
