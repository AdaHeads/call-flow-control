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

   function To_Basic_JSON
     (O : in Organization_Object)
      return JSON_Value;

   function To_Full_JSON
     (O : in Organization_Object)
      return JSON_Value;

   ---------------------
   --  To_Basic_JSON  --
   ---------------------

   function To_Basic_JSON
     (O : in Organization_Object)
      return JSON_Value
   is
      J : JSON_Value := JSON_Null;
   begin
      if O /= Null_Organization_Object then
         J := Create_Object;

         J.Set_Field (Organization_Id, Integer (O.Organization_Id));

         J.Set_Field (Full_Name, O.Full_Name);

         J.Set_Field (Identifier, O.Identifier);
      end if;

      return J;
   end To_Basic_JSON;

   --------------------
   --  To_Full_JSON  --
   --------------------

   function To_Full_JSON
     (O : in Organization_Object)
      return JSON_Value
   is
      J : JSON_Value := JSON_Null;
   begin
      if O /= Null_Organization_Object then
         if O.JSON /= JSON_Null then
            J := O.JSON;
         else
            J := Create_Object;
         end if;

         J.Set_Field (Organization_Id, Integer (O.Organization_Id));

         J.Set_Field (Full_Name, O.Full_Name);

         J.Set_Field (Identifier, O.Identifier);

         if O.Contact_List.Length > 0 then
            J.Set_Field (Contacts, View.Contact.To_JSON (O.Contact_List));
         end if;
      end if;

      return J;
   end To_Full_JSON;

   ---------------
   --  To_JSON  --
   ---------------

   function To_JSON
     (O         : in Organization_Object;
      View_Mode : in Mode := Full)
      return JSON_Value
   is
   begin
      case View_Mode is
         when Basic =>
            return To_Basic_JSON (O);
         when Full =>
            return To_Full_JSON (O);
      end case;
   end To_JSON;

   ---------------
   --  To_JSON  --
   ---------------

   function To_JSON
     (O         : in Organization_Object;
      View_Mode : in Mode := Full)
      return JSON_String
   is
   begin
      case View_Mode is
         when Basic =>
            return To_JSON_String (To_Basic_JSON (O).Write);
         when Full =>
            return To_JSON_String (To_Full_JSON (O).Write);
      end case;
   end To_JSON;

   ---------------
   --  To_JSON  --
   ---------------

   function To_JSON
     (OL        : in Organization_List_Object;
      View_Mode : in Mode := Full)
      return JSON_Value
   is
      procedure Process
        (O : in Organization_Object);
      --  Add each Organization_Object to O_Array.

      O_Array : JSON_Array;
      J       : JSON_Value := JSON_Null;

      ---------------
      --  Process  --
      ---------------

      procedure Process
        (O : in Organization_Object)
      is
      begin
         Append (O_Array, To_JSON (O, View_Mode));
      end Process;
   begin
      if OL /= Null_Organization_List_Object then
         OL.For_Each (Process'Access);

         if Length (O_Array) > 0 then
            J := Create_Object;
            J.Set_Field (Organization_List, O_Array);
         end if;
      end if;

      return J;
   end To_JSON;

   ---------------
   --  To_JSON  --
   ---------------

   function To_JSON
     (OL        : in Organization_List_Object;
      View_Mode : in Mode := Full)
      return JSON_String
   is
   begin
      case View_Mode is
         when Basic =>
            return To_JSON_String (To_JSON (OL, View_Mode).Write);
         when Full =>
            return To_JSON_String (To_JSON (OL, View_Mode).Write);
      end case;
   end To_JSON;

end View.Organization;
