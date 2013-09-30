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

with GNATCOLL.SQL;

package SQL_Statements.Users is

   use GNATCOLL.SQL;

   User_List_Query : constant SQL_Query
     := Distinct (SQL_Select (Fields => DB.User_IDs.Name,
                              From   => DB.User_IDs));

   OpenID_List_Query : constant SQL_Query
     := SQL_Select (Fields   => DB.User_IDs.OpenID,
                    From     => DB.User_IDs,
                    Where    => DB.User_IDs.Name = Text_Param (1),
                    Order_By => DB.User_IDs.Priority);

   Permission_List_Query : constant SQL_Query
     := SQL_Select (Fields   => DB.Users.Is_Receptionist &
                                DB.Users.Is_Service_Agent &
                                DB.Users.Is_Administrator,
                    From     => DB.Users,
                    Where    => DB.Users.Name = Text_Param (1));

   Permissions_By_ID : constant SQL_Query
     := SQL_Select (Fields   => DB.Users.Is_Receptionist &
                                DB.Users.Is_Service_Agent &
                                DB.Users.Is_Administrator,
                    From     => DB.User_IDs & DB.Users,
                    Where    => DB.User_IDs.Name   = DB.Users.Name and
                                DB.User_IDs.OpenID = Text_Param (1));

end SQL_Statements.Users;
