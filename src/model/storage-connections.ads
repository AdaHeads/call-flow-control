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

   function Get
     (As : in Connected_Mode)
      return Instance;
   --  TODO write comment

   procedure Queue_Failed
     (Connection : in out Instance);
   --  TODO write comment

   procedure Stop_Maintenance_Task;
   --  TODO write comment

end Storage.Connections;
