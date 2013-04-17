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

package SQL_Statements.Dial_Plans is

   use GNATCOLL.SQL;

   Get_All      : constant SQL_Query :=
     SQL_Select (Fields => DB.Dial_Plans.Phone_Number &
                           DB.Dial_Plans.Dial_Plan,
                 From   => DB.Dial_Plans);

   Get_Specific : constant SQL_Query :=
     SQL_Select (Fields => DB.Dial_Plans.Dial_Plan,
                 From   => DB.Dial_Plans,
                 Where  => DB.Dial_Plans.Phone_Number = Text_Param (1));

end SQL_Statements.Dial_Plans;
