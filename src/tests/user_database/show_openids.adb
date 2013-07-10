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
  Model.User;

procedure Show_OpenIDs is
   use Ada.Text_IO;
   use Model;

   Tux : constant User.Name := "Tux";
begin
   Put_Line ("--  OpenIDs for user """ & String (Tux) & """:");
   declare
      OpenIDs : User.OpenID_List := User.OpenIDs (Tux);
   begin
      for OpenID of OpenIDs loop
         Put_Line (User.URL (OpenID));
      end loop;
   end;
   --  for OpenID of User.OpenIDs (Tux) loop
   --     null; --  Put_Line (User.URL (OpenID));
   --  end loop;
end Show_OpenIDs;
