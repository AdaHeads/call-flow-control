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

with SQL_Prepared_Statements.Configuration,
     SQL_Statements.Dial_Plans;

package SQL_Prepared_Statements.Dial_Plans is

   use GNATCOLL.SQL.Exec;

   Get_All      : constant Prepared_Statement
     := Prepare (Query         => SQL_Statements.Dial_Plans.Get_All,
                 Auto_Complete => True,
                 On_Server     => Configuration.On_Server,
                 Name          => "all_dial_plans");

   Get_Specific : constant Prepared_Statement
     := Prepare (Query         => SQL_Statements.Dial_Plans.Get_Specific,
                 Auto_Complete => True,
                 On_Server     => Configuration.On_Server,
                 Name          => "specific_dial_plan");

end SQL_Prepared_Statements.Dial_Plans;
