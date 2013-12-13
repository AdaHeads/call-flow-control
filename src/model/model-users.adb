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

   function List (C : in out Database_Cursor'Class) return Instance;
   function List (C : in out Database_Cursor'Class) return Instance is
   begin
      return Result : Instance do
         while C.Has_Row loop
            Result.Insert (User.Create (Value (C, 0), Natural'Value (Value (C, 1))));
            C.Next;
         end loop;
      end return;
   end List;

   procedure User_Objects is
      new Storage.Process_Select_Query
            (Element           => Instance,
             Database_Cursor   => Database_Cursor,
             Cursor_To_Element => List);

   function List return Instance is
      use GNATCOLL.SQL.Exec;

      Result : Instance;

      procedure Copy (E : in Instance);
      procedure Copy (E : in Instance) is
      begin
         Result := E;
      end Copy;
   begin
      User_Objects
        (Process_Element    => Copy'Access,
         Prepared_Statement => SQL_Prepared_Statements.Users.List);

      return Result;
   end List;

end Model.Users;
