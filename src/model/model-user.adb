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

with Ada.Characters.Handling;

with SQL_Prepared_Statements.Users,
     Storage,
     Storage.Fixes;

package body Model.User is

   function User_Name (Object : in Instance) return String is
   begin
      return To_String (Object.Name);
   end User_Name;

   function ID (Object : in Instance) return Natural is
   begin
      return Object.ID;
   end ID;

   function "<" (Left, Right : in Instance) return Boolean is
   begin
      return Left.ID < Right.ID;
   end "<";

   function "=" (Left, Right : in Instance) return Boolean is
   begin
      return Left.ID = Right.ID;
   end "=";

   function Create (Name : in String;
                    ID   : in Natural) return Instance is
   begin
      return (Name => To_Unbounded_String (Name), ID => ID);
   end Create;

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
      use Ada.Characters.Handling;
   begin
      if To_Lower (C.Field_Name (0)) = "is_receptionist"  and
         To_Lower (C.Field_Name (1)) = "is_service_agent" and
         To_Lower (C.Field_Name (2)) = "is_administrator" then

         return Result : Permission_List do
            Result := (Receptionist  => Storage.Fixes.Value (C, 0),
                       Service_Agent => Storage.Fixes.Value (C, 1),
                       Administrator => Storage.Fixes.Value (C, 2));
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

   procedure Permissions is
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
      Permissions
        (Process_Element    => Copy'Access,
         Prepared_Statement => SQL_Prepared_Statements.Users.Permissions,
         Query_Parameters   => (1 => +User_Name'Access));

      if Results = 0 then
         return (others => False);
      elsif Results = 1 then
         return Result;
      else
         raise Constraint_Error
           with "Expected zero or one rows from the database when querying " &
                "permisions by user name.  Got" & Natural'Image (Results) &
                " rows.";
      end if;
   end Permissions;

   function Permissions (ID : in     OpenID) return Permission_List is
      use GNATCOLL.SQL.Exec;

      Result  : Permission_List := (others => False);
      Results : Natural := 0;

      procedure Copy (E : in Permission_List);
      procedure Copy (E : in Permission_List) is
      begin
         Result := E;
         Results := Results + 1;
      end Copy;

      ID_String : aliased constant String := URL (ID);
   begin
      Permissions
        (Process_Element    => Copy'Access,
         Prepared_Statement => SQL_Prepared_Statements.Users.Permissions_By_ID,
         Query_Parameters   => (1 => +ID_String'Access));

      if Results = 0 then
         return (others => False);
      elsif Results = 1 then
         return Result;
      else
         raise Constraint_Error
           with "Expected zero or one rows from the database when querying " &
                "permisions by OpenID.  Got" & Natural'Image (Results) &
                " rows.";
      end if;
   end Permissions;

end Model.User;
