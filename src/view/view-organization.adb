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

with View.Contact;

package body View.Organization is

   ---------------
   --  To_JSON  --
   ---------------

   function To_JSON
     (Organization : in Organization_Object)
      return JSON_Value
   is
      C_Array : JSON_Array;
      J       : JSON_Value;
   begin
      J := Create_Object;

      if Organization /= Null_Organization_Object then
         J := Organization.JSON;

         J.Set_Field (Organization_Id, Integer (Organization.Organization_Id));

         J.Set_Field (Full_Name, Organization.Full_Name);

         J.Set_Field (Identifier, Organization.Identifier);

         for Elem of Organization.Contacts loop
            Append (C_Array, View.Contact.To_JSON (Elem));
         end loop;

         if Length (C_Array) > 0 then
            J.Set_Field (Contacts, C_Array);
         end if;
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
