-------------------------------------------------------------------------------
--                                                                           --
--                      Copyright (C) 2013-, AdaHeads K/S                    --
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

with
  Ada.Text_IO;
with
  Model.User,
  Model.Users;

procedure Show_Permissions is
   use Ada.Text_IO;
   use Model;
   use type User.Permission_List;

   Permissions : User.Permission_List := (others => False);
begin
   for User_Name of Users.List loop
      Put_Line (String (User_Name) & " ...");

      Permissions := User.Permissions (User_Name);

      if Permissions = User.Permission_List'(others => False) then
         Put_Line ("   has no permissions.");
      else
         for Kind in Permissions'Range loop
            if Permissions (Kind) then
               Put_Line ("   is a " & User.Permission'Image (Kind));
            end if;
         end loop;
      end if;
   end loop;

   declare
      OpenID : User.OpenID :=
                 User.Parse ("https://accounts.google.com/we-love-tux");
   begin
      Put_Line ("<" & User.URL (OpenID) & "> ...");

      Permissions := User.Permissions (OpenID);

      if Permissions = User.Permission_List'(others => False) then
         Put_Line ("   has no permissions.");
      else
         for Kind in Permissions'Range loop
            if Permissions (Kind) then
               Put_Line ("   is a " & User.Permission'Image (Kind));
            end if;
         end loop;
      end if;
   end;
end Show_Permissions;
