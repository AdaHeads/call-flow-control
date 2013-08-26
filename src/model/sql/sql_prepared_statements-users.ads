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
     SQL_Statements.Users;

package SQL_Prepared_Statements.Users is

   use GNATCOLL.SQL.Exec;

   List : constant Prepared_Statement
     := Prepare (Query         => SQL_Statements.Users.User_List_Query,
                 Auto_Complete => True,
                 On_Server     => Configuration.On_Server,
                 Name          => "user_list");

   OpenIDs : constant Prepared_Statement
     := Prepare (Query         => SQL_Statements.Users.OpenID_List_Query,
                 Auto_Complete => True,
                 On_Server     => Configuration.On_Server,
                 Name          => "openid_list");

   Permissions : constant Prepared_Statement
     := Prepare (Query         => SQL_Statements.Users.Permission_List_Query,
                 Auto_Complete => True,
                 On_Server     => Configuration.On_Server,
                 Name          => "permission_list");

   Permissions_By_ID : constant Prepared_Statement
     := Prepare (Query         => SQL_Statements.Users.Permissions_By_ID,
                 Auto_Complete => True,
                 On_Server     => Configuration.On_Server,
                 Name          => "permissions_by_id");

end SQL_Prepared_Statements.Users;
