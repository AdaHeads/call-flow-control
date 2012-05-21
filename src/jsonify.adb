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

with GNATCOLL.JSON;

package body JSONIFY is

   ---------------
   --  Contact  --
   ---------------

   procedure Contact
     (Cursor : in out GNATCOLL.SQL.Exec.Forward_Cursor;
      Value  : in out JSON.Bounded_String)
   is
      use GNATCOLL.JSON;

      DB_Columns : JSON_Value;
      J          : JSON_Value := Create_Object;
   begin
      if Cursor.Has_Row then
         DB_Columns := Create_Object;

         J := GNATCOLL.JSON.Read (Cursor.Value (0), "json.error");

         DB_Columns.Set_Field (Cursor.Field_Name (1),
                               Cursor.Integer_Value (1));

         DB_Columns.Set_Field (Cursor.Field_Name (2),
                               Cursor.Value (2));

         DB_Columns.Set_Field (Cursor.Field_Name (3),
                               Cursor.Boolean_Value (3));

         if Cursor.Boolean_Value (3) then
            J.Set_Field ("type", "human");
         else
            J.Set_Field ("type", "function");
         end if;

         J.Set_Field ("db_columns", DB_Columns);
      end if;

      Value := JSON.To_Bounded_String (J.Write);
   end Contact;

   --------------------------
   --  Contact_Attributes  --
   --------------------------

   procedure Contact_Attributes
     (Cursor : in out GNATCOLL.SQL.Exec.Forward_Cursor;
      Value  : in out JSON.Bounded_String)
   is
      use GNATCOLL.JSON;

      Attr_Array : JSON_Array;
      DB_Columns : JSON_Value;
      DB_JSON    : JSON_Value;
      J          : constant JSON_Value := Create_Object;
   begin
      while Cursor.Has_Row loop
         DB_Columns := Create_Object;
         DB_JSON    := Create_Object;

         DB_JSON := GNATCOLL.JSON.Read (Cursor.Value (0), "json.error");

         DB_Columns.Set_Field (Cursor.Field_Name (1),
                               Cursor.Integer_Value (1));

         DB_Columns.Set_Field (Cursor.Field_Name (2),
                               Cursor.Integer_Value (2));

         DB_JSON.Set_Field ("db_columns", DB_Columns);

         Append (Attr_Array, DB_JSON);

         Cursor.Next;
      end loop;

      J.Set_Field ("attributes", Attr_Array);

      Value := JSON.To_Bounded_String (J.Write);
   end Contact_Attributes;

   --------------------
   --  Contact_Full  --
   --------------------

   procedure Contact_Full
     (Cursor : in out GNATCOLL.SQL.Exec.Forward_Cursor;
      Value  : in out JSON.Bounded_String)
   is
      use GNATCOLL.JSON;

      Attr_Array      : JSON_Array;
      Attr_DB_Columns : JSON_Value;
      Attr_JSON       : JSON_Value;
      DB_Columns      : JSON_Value;
      J               : JSON_Value := Create_Object;
   begin
      if Cursor.Has_Row then
         --  Cursor can contain more than one row, so we start by building the
         --  main JSON object from the first row, so we don't repeat the JSON
         --  building code for the same data over and over.
         DB_Columns := Create_Object;

         J := GNATCOLL.JSON.Read (Cursor.Value (0), "json.error");

         DB_Columns.Set_Field (Cursor.Field_Name (1),
                               Cursor.Integer_Value (1));

         DB_Columns.Set_Field (Cursor.Field_Name (2),
                               Cursor.Value (2));

         DB_Columns.Set_Field (Cursor.Field_Name (3),
                               Cursor.Boolean_Value (3));

         if Cursor.Boolean_Value (3) then
            J.Set_Field ("type", "human");
         else
            J.Set_Field ("type", "function");
         end if;

         J.Set_Field ("db_columns", DB_Columns);

         while Cursor.Has_Row loop
            Attr_JSON := Create_Object;
            Attr_DB_Columns := Create_Object;

            Attr_JSON := GNATCOLL.JSON.Read (Cursor.Value (4), "json.error");

            Attr_DB_Columns.Set_Field (Cursor.Field_Name (5),
                                       Cursor.Integer_Value (5));

            Attr_DB_Columns.Set_Field (Cursor.Field_Name (6),
                                       Cursor.Integer_Value (6));

            Attr_JSON.Set_Field ("db_columns", Attr_DB_Columns);

            Append (Attr_Array, Attr_JSON);

            Cursor.Next;
         end loop;

         J.Set_Field ("attributes", Attr_Array);
      end if;

      Value := JSON.To_Bounded_String (J.Write);
   end Contact_Full;

   --------------------
   --  Org_Contacts  --
   --------------------

   procedure Org_Contacts
     (Cursor : in out GNATCOLL.SQL.Exec.Forward_Cursor;
      Value  : in out JSON.Bounded_String)
   is
      use GNATCOLL.JSON;

      Contact_Array : JSON_Array;
      DB_Columns    : JSON_Value;
      DB_JSON       : JSON_Value;
      J             : constant JSON_Value := Create_Object;
   begin
      while Cursor.Has_Row loop
         DB_Columns := Create_Object;
         DB_JSON    := Create_Object;

         DB_JSON := GNATCOLL.JSON.Read (Cursor.Value (0), "json.error");

         DB_Columns.Set_Field (Cursor.Field_Name (1),
                               Cursor.Integer_Value (1));

         DB_Columns.Set_Field (Cursor.Field_Name (2),
                               Cursor.Value (2));

         DB_Columns.Set_Field (Cursor.Field_Name (3),
                               Cursor.Boolean_Value (3));

         if Cursor.Boolean_Value (3) then
            DB_JSON.Set_Field ("type", "human");
         else
            DB_JSON.Set_Field ("type", "function");
         end if;

         DB_JSON.Set_Field ("db_columns", DB_Columns);

         Append (Contact_Array, DB_JSON);

         Cursor.Next;
      end loop;

      J.Set_Field ("contacts", Contact_Array);

      Value := JSON.To_Bounded_String (J.Write);
   end Org_Contacts;

   -------------------------------
   --  Org_Contacts_Attributes  --
   -------------------------------

   procedure Org_Contacts_Attributes
     (Cursor : in out GNATCOLL.SQL.Exec.Forward_Cursor;
      Value  : in out JSON.Bounded_String)
   is
      use GNATCOLL.JSON;

      Attr_Array : JSON_Array;
      DB_Columns : JSON_Value;
      DB_JSON    : JSON_Value;
      J          : constant JSON_Value := Create_Object;
   begin
      while Cursor.Has_Row loop
         DB_Columns := Create_Object;
         DB_JSON    := Create_Object;

         DB_JSON := GNATCOLL.JSON.Read (Cursor.Value (0), "json.error");

         DB_Columns.Set_Field (Cursor.Field_Name (1),
                               Cursor.Integer_Value (1));

         DB_Columns.Set_Field (Cursor.Field_Name (2),
                               Cursor.Integer_Value (2));

         DB_JSON.Set_Field ("db_columns", DB_Columns);

         Append (Attr_Array, DB_JSON);

         Cursor.Next;
      end loop;

      J.Set_Field ("attributes", Attr_Array);

      Value := JSON.To_Bounded_String (J.Write);
   end Org_Contacts_Attributes;

   -------------------------
   --  Org_Contacts_Full  --
   -------------------------

   procedure Org_Contacts_Full
     (Cursor : in out GNATCOLL.SQL.Exec.Forward_Cursor;
      Value  : in out JSON.Bounded_String)
   is
      use GNATCOLL.JSON;

      Attr_DB_Columns : JSON_Value;
      Attr_JSON       : JSON_Value;
      Contact_Array   : JSON_Array;
      Contact_JSON    : JSON_Value;
      DB_Columns      : JSON_Value;
      J               : constant JSON_Value := Create_Object;
   begin
      while Cursor.Has_Row loop
         Contact_JSON := Create_Object;
         DB_Columns := Create_Object;

         Contact_JSON := GNATCOLL.JSON.Read (Cursor.Value (0), "json.error");

         DB_Columns.Set_Field (Cursor.Field_Name (1),
                               Cursor.Integer_Value (1));

         DB_Columns.Set_Field (Cursor.Field_Name (2),
                               Cursor.Value (2));

         DB_Columns.Set_Field (Cursor.Field_Name (3),
                               Cursor.Boolean_Value (3));

         if Cursor.Boolean_Value (3) then
            Contact_JSON.Set_Field ("type", "human");
         else
            Contact_JSON.Set_Field ("type", "function");
         end if;

         Contact_JSON.Set_Field ("db_columns", DB_Columns);

         Attr_JSON := Create_Object;
         Attr_DB_Columns := Create_Object;

         Attr_JSON := GNATCOLL.JSON.Read (Cursor.Value (4), "json.error");

         Attr_DB_Columns.Set_Field (Cursor.Field_Name (5),
                                    Cursor.Integer_Value (5));

         Attr_DB_Columns.Set_Field (Cursor.Field_Name (6),
                                    Cursor.Integer_Value (6));

         Attr_JSON.Set_Field ("db_columns", Attr_DB_Columns);

         Contact_JSON.Set_Field ("attributes", Attr_JSON);

         Append (Contact_Array, Contact_JSON);

         Cursor.Next;
      end loop;

      J.Set_Field ("contacts", Contact_Array);

      Value := JSON.To_Bounded_String (J.Write);
   end Org_Contacts_Full;

   --------------------
   --  Organization  --
   --------------------

   procedure Organization
     (Cursor : in out GNATCOLL.SQL.Exec.Forward_Cursor;
      Value  : in out JSON.Bounded_String)
   is
      use GNATCOLL.JSON;

      DB_Columns : JSON_Value;
      J          : JSON_Value := Create_Object;
   begin
      if Cursor.Has_Row then
         DB_Columns := Create_Object;

         J := GNATCOLL.JSON.Read (Cursor.Value (0), "json.error");

         DB_Columns.Set_Field (Cursor.Field_Name (1),
                               Cursor.Integer_Value (1));

         DB_Columns.Set_Field (Cursor.Field_Name (2),
                               Cursor.Value (2));

         DB_Columns.Set_Field (Cursor.Field_Name (3),
                               Cursor.Value (3));

         J.Set_Field ("db_columns", DB_Columns);
      end if;

      Value := JSON.To_Bounded_String (J.Write);
   end Organization;

end JSONIFY;
