-------------------------------------------------------------------------------
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

   function To_Midi_JSON
     (O : in Model.Organization.Organization_Object)
      return JSON_Value;

   function To_Mini_JSON
     (O : in Model.Organization.Organization_Object)
      return JSON_Value;

   function To_Maxi_JSON
     (O : in Model.Organization.Organization_Object)
      return JSON_Value;

   ---------------
   --  To_JSON  --
   ---------------

   function To_JSON
     (O : in Model.Organization.Organization_Object)
      return JSON_Value
   is
      use Model.Organization;
   begin
      case O.Mode is
         when Mini =>
            return To_Mini_JSON (O);
         when Midi =>
            return To_Midi_JSON (O);
         when Maxi =>
            return To_Maxi_JSON (O);
      end case;
   end To_JSON;

   ---------------
   --  To_JSON  --
   ---------------

   function To_JSON
     (Instance : in Model.Organizations.Organization_List_Object)
      return JSON_Value
   is
      procedure Process
        (O : in Model.Organization.Organization_Object);
      --  Add each Organization_Object to O_Array.

      O_Array : JSON_Array;
      J       : JSON_Value := JSON_Null;

      ---------------
      --  Process  --
      ---------------

      procedure Process
        (O : in Model.Organization.Organization_Object)
      is
      begin
         Append (O_Array, To_JSON (O));
      end Process;
   begin
      Instance.For_Each (Process'Access);

      if Length (O_Array) > 0 then
         J := Create_Object;
         J.Set_Field (Organization_List, O_Array);
      end if;

      return J;
   end To_JSON;

   ----------------------
   --  To_JSON_String  --
   ----------------------

   function To_JSON_String
     (Instance  : in Model.Organizations.Organization_List_Object)
      return JSON_String
   is
   begin
      return To_JSON_String (To_JSON (Instance).Write);
   end To_JSON_String;

   ----------------------
   --  To_JSON_String  --
   ----------------------

   function To_JSON_String
     (Instance  : in Model.Organization.Organization_Object)
      return JSON_String
   is
      use Model.Organization;
   begin
      case Instance.Mode is
         when Mini =>
            return To_JSON_String (To_Mini_JSON (Instance).Write);
         when Midi =>
            return To_JSON_String (To_Midi_JSON (Instance).Write);
         when Maxi =>
            return To_JSON_String (To_Maxi_JSON (Instance).Write);
      end case;
   end To_JSON_String;

   --------------------
   --  To_Maxi_JSON  --
   --------------------

   function To_Maxi_JSON
     (O : in Model.Organization.Organization_Object)
      return JSON_Value
   is
      use Model.Organization;

      C_Array : JSON_Array;
      J       : JSON_Value := JSON_Null;
   begin
      if O /= Null_Organization then
         if O.JSON /= JSON_Null then
            J := O.JSON;
         else
            J := Create_Object;
         end if;

         J.Set_Field (Organization_Id, Integer (O.ID));

         J.Set_Field (Full_Name, O.Full_Name);

         J.Set_Field (Identifier, O.Identifier);

         C_Array := O.Contact_List.To_JSON_Array;

         if Length (C_Array) > 0 then
            J.Set_Field (Contacts, C_Array);
         end if;
      end if;

      return J;
   end To_Maxi_JSON;

   --------------------
   --  To_Midi_JSON  --
   --------------------

   function To_Midi_JSON
     (O : in Model.Organization.Organization_Object)
      return JSON_Value
   is
      use Model.Organization;

      J : JSON_Value := JSON_Null;
   begin
      if O /= Null_Organization then
         if O.JSON /= JSON_Null then
            J := O.JSON;
         else
            J := Create_Object;
         end if;

         J.Set_Field (Organization_Id, Integer (O.ID));

         J.Set_Field (Full_Name, O.Full_Name);

         J.Set_Field (Identifier, O.Identifier);
      end if;

      return J;
   end To_Midi_JSON;

   --------------------
   --  To_Mini_JSON  --
   --------------------

   function To_Mini_JSON
     (O : in Model.Organization.Organization_Object)
      return JSON_Value
   is
      use Model.Organization;

      J : JSON_Value := JSON_Null;
   begin
      if O /= Null_Organization then
         J := Create_Object;

         J.Set_Field (Organization_Id, Integer (O.ID));

         J.Set_Field (Full_Name, O.Full_Name);

         J.Set_Field (Identifier, O.Identifier);
      end if;

      return J;
   end To_Mini_JSON;

end View.Organization;
