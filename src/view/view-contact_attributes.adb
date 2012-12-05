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
     (O : in Contact_Attributes_List_Object)
      return JSON_Array
   is
      procedure Add_Attribute_To_Array
        (Elem : in Contact_Attributes_Object);

      A_Array : JSON_Array := Empty_Array;

      ------------------------------
      --  Add_Attribute_To_Array  --
      ------------------------------

      procedure Add_Attribute_To_Array
        (Elem : in Contact_Attributes_Object)
      is
      begin
         Append (A_Array, To_JSON (Elem));
      end Add_Attribute_To_Array;
   begin
      if O /= Null_Contact_Attributes_List then
         O.For_Each (Add_Attribute_To_Array'Access);
      end if;

      return A_Array;
   end To_JSON;

   ---------------
   --  To_JSON  --
   ---------------

   function To_JSON
     (O : in Contact_Attributes_Object)
      return JSON_Value
   is
      J : JSON_Value := JSON_Null;
   begin
      if O.JSON /= JSON_Null then
         J := O.JSON;
      else
         J := Create_Object;
      end if;

      J.Set_Field (Contact_Id, Integer (O.Contact_ID));

      J.Set_Field (Organization_Id,
                   Integer (O.Organization_ID));

      return J;
   end To_JSON;

   ---------------
   --  To_JSON  --
   ---------------

   function To_JSON
     (O : in Contact_Attributes_Object)
      return JSON_String
   is
   begin
      return To_JSON_String (To_JSON (O).Write);
   end To_JSON;

end View.Contact_Attributes;
