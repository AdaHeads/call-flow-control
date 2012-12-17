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

with Request_Parameters;

package body View.Organization is

   function To_Midi_JSON
     (Instance : in Model.Organization.Object)
      return JSON_Value;
   --  Convert Instance to a "midi" JSON object.
   --  See Model.Organization.Data_Mode for an explanation of the different
   --  "views".

   function To_Mini_JSON
     (Instance : in Model.Organization.Object)
      return JSON_Value;
   --  Convert Instance to a "mini" JSON object.
   --  See Model.Organization.Data_Mode for an explanation of the different
   --  "views".

   function To_Maxi_JSON
     (Instance : in Model.Organization.Object)
      return JSON_Value;
   --  Convert Instance to a "maxi" JSON object.
   --  See Model.Organization.Data_Mode for an explanation of the different
   --  "views".

   ---------------
   --  To_JSON  --
   ---------------

   function To_JSON
     (Instance : in Model.Organization.Object)
      return JSON_Value
   is
      use Model.Organization;
   begin
      case Instance.Mode is
         when Request_Parameters.Mini =>
            return To_Mini_JSON (Instance);
         when Request_Parameters.Midi =>
            return To_Midi_JSON (Instance);
         when Request_Parameters.Maxi =>
            return To_Maxi_JSON (Instance);
      end case;
   end To_JSON;

   ---------------
   --  To_JSON  --
   ---------------

   function To_JSON
     (Instance : in Model.Organizations.List)
      return JSON_Value
   is
      procedure Process
        (O : in Model.Organization.Object);
      --  Add each Organization_Object to O_Array.

      O_Array : JSON_Array;
      J       : JSON_Value := JSON_Null;

      ---------------
      --  Process  --
      ---------------

      procedure Process
        (O : in Model.Organization.Object)
      is
      begin
         Append (O_Array, To_JSON (O));
      end Process;
   begin
      Instance.For_Each (Process'Access);

      if GNATCOLL.JSON.Length (O_Array) > 0 then
         J := Create_Object;
         J.Set_Field (Organization_List, O_Array);
      end if;

      return J;
   end To_JSON;

   ----------------------
   --  To_JSON_String  --
   ----------------------

   function To_JSON_String
     (Instance  : in Model.Organizations.List)
      return JSON_String
   is
   begin
      return To_JSON_String (To_JSON (Instance).Write);
   end To_JSON_String;

   ----------------------
   --  To_JSON_String  --
   ----------------------

   function To_JSON_String
     (Instance  : in Model.Organization.Object)
      return JSON_String
   is
      use Model.Organization;
   begin
      case Instance.Mode is
         when Request_Parameters.Mini =>
            return To_JSON_String (To_Mini_JSON (Instance).Write);
         when Request_Parameters.Midi =>
            return To_JSON_String (To_Midi_JSON (Instance).Write);
         when Request_Parameters.Maxi =>
            return To_JSON_String (To_Maxi_JSON (Instance).Write);
      end case;
   end To_JSON_String;

   --------------------
   --  To_Maxi_JSON  --
   --------------------

   function To_Maxi_JSON
     (Instance : in Model.Organization.Object)
      return JSON_Value
   is
      use Model.Organization;

      C_Array : JSON_Array;
      J       : JSON_Value := JSON_Null;
   begin
      if Instance /= Null_Organization then
         if Instance.JSON /= JSON_Null then
            J := Instance.JSON;
         else
            J := Create_Object;
         end if;

         J.Set_Field (Organization_Id, Integer (Instance.ID));

         J.Set_Field (Full_Name, Instance.Full_Name);

         J.Set_Field (Identifier, Instance.Identifier);

         C_Array := Instance.Contact_List.To_JSON_Array;

         if GNATCOLL.JSON.Length (C_Array) > 0 then
            J.Set_Field (Contacts, C_Array);
         end if;
      end if;

      return J;
   end To_Maxi_JSON;

   --------------------
   --  To_Midi_JSON  --
   --------------------

   function To_Midi_JSON
     (Instance : in Model.Organization.Object)
      return JSON_Value
   is
      use Model.Organization;

      J : JSON_Value := JSON_Null;
   begin
      if Instance /= Null_Organization then
         if Instance.JSON /= JSON_Null then
            J := Instance.JSON;
         else
            J := Create_Object;
         end if;

         J.Set_Field (Organization_Id, Integer (Instance.ID));

         J.Set_Field (Full_Name, Instance.Full_Name);

         J.Set_Field (Identifier, Instance.Identifier);
      end if;

      return J;
   end To_Midi_JSON;

   --------------------
   --  To_Mini_JSON  --
   --------------------

   function To_Mini_JSON
     (Instance : in Model.Organization.Object)
      return JSON_Value
   is
      use Model.Organization;

      J : JSON_Value := JSON_Null;
   begin
      if Instance /= Null_Organization then
         J := Create_Object;

         J.Set_Field (Organization_Id, Integer (Instance.ID));

         J.Set_Field (Full_Name, Instance.Full_Name);

         J.Set_Field (Identifier, Instance.Identifier);
      end if;

      return J;
   end To_Mini_JSON;

end View.Organization;
