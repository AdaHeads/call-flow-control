-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                            View.Organization                              --
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
   --  Create and return a basic organization JSON document. This only contains
   --  id, full name and string identifier. No contacts.

   function To_Full_JSON
     (O : in Organization_Object)
      return JSON_Value;
   --  Create and return a full organization JSON document. This contains all
   --  organization related data and contacts, if there are any.

   ---------------------
   --  To_Basic_JSON  --
   ---------------------

   function To_Basic_JSON
     (O : in Organization_Object)
      return JSON_Value
   is
      J : JSON_Value := JSON_Null;
   begin
      if O /= Null_Organization then
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
      C_Array : JSON_Array;
      J       : JSON_Value := JSON_Null;
   begin
      if O /= Null_Organization then
         if O.JSON /= JSON_Null then
            J := O.JSON;
         else
            J := Create_Object;
         end if;

         J.Set_Field (Organization_Id, Integer (O.Organization_Id));

         J.Set_Field (Full_Name, O.Full_Name);

         J.Set_Field (Identifier, O.Identifier);

         C_Array := View.Contact.To_JSON (O.Contact_List);

         if Length (C_Array) > 0 then
            J.Set_Field (Contacts, C_Array);
         end if;
      end if;

      return J;
   end To_Full_JSON;

   ---------------
   --  To_JSON  --
   ---------------

   function To_JSON
     (O    : in Organization_Object;
      View : in Mode)
      return JSON_Value
   is
   begin
      case View is
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
     (O    : in Organization_Object;
      View : in Mode)
      return JSON_String
   is
   begin
      case View is
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
     (O    : in Organization_List_Object;
      View : in Mode)
      return JSON_Value
   is
      procedure Process
        (O : in Organization_Object'Class);
      --  Add each Organization_Object to O_Array.

      O_Array : JSON_Array;
      J       : JSON_Value := JSON_Null;

      ---------------
      --  Process  --
      ---------------

      procedure Process
        (O : in Organization_Object'Class)
      is
      begin
         Append (O_Array, To_JSON (Organization_Object (O), View));
      end Process;
   begin
      O.For_Each (Process'Access);

      if Length (O_Array) > 0 then
         J := Create_Object;
         J.Set_Field (Organization_List, O_Array);
      end if;

      return J;
   end To_JSON;

   ---------------
   --  To_JSON  --
   ---------------

   function To_JSON
     (O    : in Organization_List_Object;
      View : in Mode)
      return JSON_String
   is
   begin
      return To_JSON_String (To_JSON (O, View).Write);
   end To_JSON;

end View.Organization;
