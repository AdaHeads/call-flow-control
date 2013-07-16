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

   function List (C : in out Database_Cursor'Class) return Permission_List;
   function List (C : in out Database_Cursor'Class) return Permission_List is
   begin
      if C.Field_Name (0) = "Is_Receptionist"  and
         C.Field_Name (1) = "Is_Service_Agent" and
         C.Field_Name (2) = "Is_Administrator" then

         return Result : Permission_List do
            Result := (Receptionist  => C.Boolean_Value (0),
                       Service_Agent => C.Boolean_Value (1),
                       Administrator => C.Boolean_Value (2));
            Result := (Receptionist  => Boolean'Value (C.Value (0)),
                       Service_Agent => Boolean'Value (C.Value (1)),
                       Administrator => Boolean'Value (C.Value (2)));
            C.Next;
         end return;
      else
         raise Constraint_Error
           with "Unexpected column names from the database: (" &
                C.Field_Name (0) & ", " &
                C.Field_Name (1) & ", " &
                C.Field_Name (2) & ")";
      end if;
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

   procedure Permissions_For_User is
      new Storage.Process_Select_Query
            (Element           => Permission_List,
             Database_Cursor   => Database_Cursor,
             Cursor_To_Element => List);

   function Permissions (User : in     Name) return Permission_List is
      use GNATCOLL.SQL.Exec;

      Result  : Permission_List := (others => False);
      Results : Natural := 0;

      procedure Copy (E : in Permission_List);
      procedure Copy (E : in Permission_List) is
      begin
         Result := E;
         Results := Results + 1;
      end Copy;

      User_Name : aliased constant String := String (User);
   begin
      Permissions_For_User
        (Process_Element    => Copy'Access,
         Prepared_Statement => SQL_Prepared_Statements.Users.Permissions,
         Query_Parameters   => (1 => +User_Name'Access));

      if Results = 1 then
         return Result;
      else
         raise Constraint_Error
           with "Expected exactly one row from the database.";
      end if;
   end Permissions;

end Model.User;
