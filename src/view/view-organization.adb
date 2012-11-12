-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                            View.Organizations                             --
--                                                                           --
--                                  BODY                                     --
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

package body View.Organization is

   ---------------
   --  To_JSON  --
   ---------------

   function To_JSON
     (Organization : in Organization_Object)
      return JSON_Value
   is
      J : JSON_Value;
   begin
      J := Create_Object;

      if Organization /= Null_Organization_Object then
         J := Organization.JSON;

         J.Set_Field ("organization_id",
                      Integer (Organization.Organization_Id));

         J.Set_Field ("full_name",
                      Organization.Full_Name);

         J.Set_Field ("identifier",
                      Organization.Identifier);
      end if;

      return J;
   end To_JSON;

   ---------------
   --  To_JSON  --
   ---------------

   function To_JSON
     (Organization : in Organization_Object)
      return JSON_String
   is
   begin
      return To_JSON_String (To_JSON (Organization).Write);
   end To_JSON;

end View.Organization;
