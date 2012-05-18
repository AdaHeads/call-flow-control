-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                 JSONIFY                                   --
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

with Yolk.Utilities;

package body JSONIFY is

   ---------------
   --  Contact  --
   ---------------

   function Contact
     (Cursor : in Exec.Forward_Cursor)
      return Unbounded_String
   is
      use Yolk.Utilities;

      DB_Columns : JSON_Value;
      JSON       : JSON_Value := Create_Object;
   begin
      if Cursor.Has_Row then
         DB_Columns := Create_Object;

         JSON := GNATCOLL.JSON.Read (Cursor.Value (0), "json.error");

         DB_Columns.Set_Field (Field_Name => Cursor.Field_Name (1),
                               Field      => Cursor.Integer_Value (1));

         DB_Columns.Set_Field (Field_Name => Cursor.Field_Name (2),
                               Field      => Cursor.Value (2));

         DB_Columns.Set_Field (Field_Name => Cursor.Field_Name (3),
                               Field      => Cursor.Boolean_Value (3));

         if Cursor.Boolean_Value (3) then
            JSON.Set_Field (Field_Name => "type",
                            Field      => "human");
         else
            JSON.Set_Field (Field_Name => "type",
                            Field      => "function");
         end if;

         JSON.Set_Field (Field_Name => "db_columns",
                         Field      => DB_Columns);
      end if;

      return TUS (JSON.Write);
   end Contact;

   --------------------------
   --  Contact_Attributes  --
   --------------------------

   function Contact_Attributes
     (Cursor : in out Exec.Forward_Cursor)
      return Unbounded_String
   is
      Attr_Array : JSON_Array;
      DB_Columns : JSON_Value;
      DB_JSON    : JSON_Value;
   begin
      while Cursor.Has_Row loop
         DB_Columns := Create_Object;
         DB_JSON    := Create_Object;

         DB_JSON := GNATCOLL.JSON.Read (Cursor.Value (0), "json.error");

         DB_Columns.Set_Field (Field_Name => Cursor.Field_Name (1),
                               Field      => Cursor.Integer_Value (1));

         DB_Columns.Set_Field (Field_Name => Cursor.Field_Name (2),
                               Field      => Cursor.Integer_Value (2));

         DB_JSON.Set_Field (Field_Name => "db_columns",
                            Field      => DB_Columns);

         Append (Arr => Attr_Array,
                 Val => DB_JSON);

         Cursor.Next;
      end loop;

      JSON.Set_Field (Field_Name => "attributes",
                      Field      => Attr_Array);
   end Contact_Attributes;

   --------------------
   --  Contact_Full  --
   --------------------

   procedure Contact_Full
     (Cursor : in out Exec.Forward_Cursor)
      return Unbounded_String
   is
      DB_Columns : JSON_Value;
   begin
      if Cursor.Has_Row then

         --  BAD IMPLEMENTATION! NOT COMPLETE!

         DB_Columns := Create_Object;

         JSON := GNATCOLL.JSON.Read (Cursor.Value (0), "json.error");

         DB_Columns.Set_Field (Field_Name => Cursor.Field_Name (1),
                               Field      => Cursor.Integer_Value (1));

         DB_Columns.Set_Field (Field_Name => Cursor.Field_Name (2),
                               Field      => Cursor.Value (2));

         DB_Columns.Set_Field (Field_Name => Cursor.Field_Name (3),
                               Field      => Cursor.Boolean_Value (3));

         if Cursor.Boolean_Value (3) then
            JSON.Set_Field (Field_Name => "type",
                            Field      => "human");
         else
            JSON.Set_Field (Field_Name => "type",
                            Field      => "function");
         end if;

         JSON.Set_Field (Field_Name => "db_columns",
                         Field      => DB_Columns);
      end if;
   end Contact_Full;

   --------------------
   --  Org_Contacts  --
   --------------------

   procedure Org_Contacts
     (Cursor : in out Exec.Forward_Cursor)
      return Unbounded_String
   is
      Contact_Array : JSON_Array;
      DB_Columns    : JSON_Value;
      DB_JSON       : JSON_Value;
   begin
      while Cursor.Has_Row loop
         DB_Columns := Create_Object;
         DB_JSON    := Create_Object;

         DB_JSON := GNATCOLL.JSON.Read (Cursor.Value (0), "json.error");

         DB_Columns.Set_Field (Field_Name => Cursor.Field_Name (1),
                               Field      => Cursor.Integer_Value (1));

         DB_Columns.Set_Field (Field_Name => Cursor.Field_Name (2),
                               Field      => Cursor.Integer_Value (2));

         DB_Columns.Set_Field (Field_Name => Cursor.Field_Name (3),
                               Field      => Cursor.Value (3));

         DB_Columns.Set_Field (Field_Name => Cursor.Field_Name (4),
                               Field      => Cursor.Boolean_Value (4));

         if Cursor.Boolean_Value (4) then
            DB_JSON.Set_Field (Field_Name => "type",
                               Field      => "human");
         else
            DB_JSON.Set_Field (Field_Name => "type",
                               Field      => "function");
         end if;

         DB_JSON.Set_Field (Field_Name => "db_columns",
                            Field      => DB_Columns);

         Append (Arr => Contact_Array,
                 Val => DB_JSON);

         Cursor.Next;
      end loop;

      JSON.Set_Field (Field_Name => "contacts",
                      Field      => Contact_Array);
   end Org_Contacts;

   -------------------------------
   --  Org_Contacts_Attributes  --
   -------------------------------

   procedure Org_Contacts_Attributes
     (Cursor : in out Exec.Forward_Cursor)
      return Unbounded_String
   is
      Attr_Array : JSON_Array;
      DB_Columns : JSON_Value;
      DB_JSON    : JSON_Value;
   begin
      while Cursor.Has_Row loop
         DB_Columns := Create_Object;
         DB_JSON    := Create_Object;

         DB_JSON := GNATCOLL.JSON.Read (Cursor.Value (0), "json.error");

         DB_Columns.Set_Field (Field_Name => Cursor.Field_Name (1),
                               Field      => Cursor.Integer_Value (1));

         DB_Columns.Set_Field (Field_Name => Cursor.Field_Name (2),
                               Field      => Cursor.Integer_Value (2));

         DB_JSON.Set_Field (Field_Name => "db_columns",
                            Field      => DB_Columns);

         Append (Arr => Attr_Array,
                 Val => DB_JSON);

         Cursor.Next;
      end loop;

      JSON.Set_Field (Field_Name => "attributes",
                      Field      => Attr_Array);
   end Org_Contacts_Attributes;

end JSONIFY;
