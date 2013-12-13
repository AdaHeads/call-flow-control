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
     := Distinct (SQL_Select (Fields => DB.Users.Name & DB.Users.ID,
                              From   => DB.Users));

   User_ID_Of_Identity_Query : constant SQL_Query
     := SQL_Select (Fields   => DB.Auth_Identities.User_ID,
                    From     => DB.Auth_Identities,
                    Where    => DB.Auth_Identities.Identity = Text_Param (1));
   --  Determines the User_ID of a given identity.

   OpenID_List_Query : constant SQL_Query
     := SQL_Select (Fields   => DB.User_Ids.OpenID,
                    From     => DB.User_Ids,
                    Where    => DB.User_Ids.User_Id = Integer_Param (1),
                    Order_By => DB.User_Ids.Priority);

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
                    From     => DB.User_Ids & DB.Users,
                    Where    => DB.User_Ids.User_Id = DB.Users.Id and
                      DB.User_Ids.OpenId = Text_Param (1));

end SQL_Statements.Users;
