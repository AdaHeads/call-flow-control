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

with SQL_Prepared_Statements.Users,
     Storage;

package body Model.Users is

   function List (C : in out Database_Cursor'Class) return Instance is
   begin
      return Result : Instance do
         loop
            Result.Insert (User.Name (Value (C, 0)));
            exit when not C.Has_Row;
            C.Next;
         end loop;
      end return;
   end List;

   function List return Instance is
      use GNATCOLL.SQL.Exec;
   begin
      raise Program_Error with "Model.Users.List is not implemented.";

      return User_Lists.Empty_Set;
   end List;

end Model.Users;
