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

with GNATCOLL.SQL;

package SQL_Statements.Attribute is

   use GNATCOLL.SQL;

   ----------------------------------------------------------------------------
   --  SQL for fetching the attributes of a contact.                         --
   ----------------------------------------------------------------------------

   Contact_Attributes_Query : constant SQL_Query
     := SQL_Select (Fields =>
                      DB.Contact_Attributes.Contact_Id &       --  0
                      DB.Contact_Attributes.Organization_Id &  --  1
                      DB.Contact_Attributes.Json,              --  2
                    From   => DB.Contact_Attributes);

   Contact_With_Id_Attributes_Query : constant SQL_Query
     := Where_And (Query => Contact_Attributes_Query,
                   Where =>
                     DB.Contact_Attributes.Contact_Id = Integer_Param (1));

   ----------------------------------------------------------------------------
   --  SQL for fetching the attributes of a contact related to a specific    --
   --  organization.                                                         --
   ----------------------------------------------------------------------------

   Contact_Organization_Attributes_Query : constant SQL_Query
     := Where_And (Query => Contact_Attributes_Query,
                   Where =>
                     DB.Contact_Attributes.Contact_Id = Integer_Param (1)
                   and
                     DB.Contact_Attributes.Organization_Id =
                       Integer_Param (2));

end SQL_Statements.Attribute;
