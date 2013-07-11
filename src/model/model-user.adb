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

package body Model.User is

   function List (C : in out Database_Cursor'Class) return OpenID_List;
   function List (C : in out Database_Cursor'Class) return OpenID_List is
   begin
      return Result : OpenID_List do
         while C.Has_Row loop
            Result.Append (Parse (Value (C, 0)));
            C.Next;
         end loop;
      end return;
   end List;

   procedure OpenIDs_For_User is
      new Storage.Process_Select_Query
            (Element           => OpenID_List,
             Database_Cursor   => Database_Cursor,
             Cursor_To_Element => List);

   function OpenIDs (User : in     Name) return OpenID_List is
      use GNATCOLL.SQL.Exec;

      Result : OpenID_List;

      procedure Copy (E : in OpenID_List);
      procedure Copy (E : in OpenID_List) is
      begin
         Result := E;
      end Copy;

      User_Name : aliased constant String := String (User);
   begin
      OpenIDs_For_User
        (Process_Element    => Copy'Access,
         Prepared_Statement => SQL_Prepared_Statements.Users.OpenIDs,
         Query_Parameters   => (1 => +User_Name'Access));

      return Result;
   end OpenIDs;

end Model.User;
