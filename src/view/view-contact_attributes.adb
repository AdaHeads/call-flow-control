-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                         View.Contact_Attributes                           --
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

package body View.Contact_Attributes is

   ---------------
   --  To_JSON  --
   ---------------

   function To_JSON
     (Contact_Attributes : in Contact_Attributes_Object)
      return JSON_Value
   is
      J : JSON_Value;
   begin
      J := Contact_Attributes.Get_JSON;

      J.Set_Field ("contact_id",
                   Integer (Contact_Attributes.Get_Contact_Id));

      J.Set_Field ("organization_id",
                   Integer (Contact_Attributes.Get_Organization_Id));

      return J;
   end To_JSON;

   ---------------
   --  To_JSON  --
   ---------------

   function To_JSON
     (Contact_Attributes : in Contact_Attributes_Object)
      return JSON_String
   is
   begin
      return To_JSON_String (To_JSON (Contact_Attributes).Write);
   end To_JSON;

end View.Contact_Attributes;
