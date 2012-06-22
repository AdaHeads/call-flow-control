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
with Yolk.Utilities;

package body JSONIFY is

   ---------------
   --  Contact  --
   ---------------

   procedure Contact
     (C     : in     Storage.Queries.Contact_Cursor;
      Value : in out Common.JSON_String)
   is
      use Common;
      use GNATCOLL.JSON;
      use Yolk.Utilities;

      DB_Columns : JSON_Value;
      J          : JSON_Value := Create_Object;
   begin
      if C.Has_Row then
         DB_Columns := Create_Object;

         J := GNATCOLL.JSON.Read (To_String (C.Element.JSON), "json.error");

         DB_Columns.Set_Field (TS (C.Element.Ce_Id.Name),
                               C.Element.Ce_Id.Value);

         DB_Columns.Set_Field (TS (C.Element.Ce_Name.Name),
                               C.Element.Ce_Name.Value);

         DB_Columns.Set_Field (TS (C.Element.Is_Human.Name),
                               C.Element.Is_Human.Value);

         if C.Element.Is_Human.Value then
            J.Set_Field ("type", "human");
         else
            J.Set_Field ("type", "function");
         end if;

         J.Set_Field ("db_columns", DB_Columns);
      end if;

      Value := To_JSON_String (J.Write);
   end Contact;

   --------------------------
   --  Contact_Attributes  --
   --------------------------

   procedure Contact_Attributes
     (C     : in out Storage.Queries.Contact_Attributes_Cursor;
      Value : in out Common.JSON_String)
   is
      use Common;
      use GNATCOLL.JSON;
      use Yolk.Utilities;

      Attr_Array : JSON_Array;
      DB_Columns : JSON_Value;
      DB_JSON    : JSON_Value;
      J          : constant JSON_Value := Create_Object;
   begin
      while C.Has_Row loop
         DB_Columns := Create_Object;
         DB_JSON    := Create_Object;

         DB_JSON := GNATCOLL.JSON.Read (To_String (C.Element.JSON),
                                        "db_json.json.error");

         DB_Columns.Set_Field (TS (C.Element.Ce_Id.Name),
                               C.Element.Ce_Id.Value);

         DB_Columns.Set_Field (TS (C.Element.Org_Id.Name),
                               C.Element.Org_Id.Value);

         DB_JSON.Set_Field ("db_columns", DB_Columns);

         Append (Attr_Array, DB_JSON);

         C.Next;
      end loop;

      J.Set_Field ("attributes", Attr_Array);

      Value := To_JSON_String (J.Write);
   end Contact_Attributes;

   --------------------
   --  Contact_Full  --
   --------------------

   procedure Contact_Full
     (C     : in out Storage.Queries.Contact_Full_Cursor;
      Value : in out Common.JSON_String)
   is
      use Common;
      use GNATCOLL.JSON;
      use Yolk.Utilities;

      Attr_Array      : JSON_Array;
      Attr_DB_Columns : JSON_Value;
      Attr_JSON       : JSON_Value;
      DB_Columns      : JSON_Value;
      J               : JSON_Value := Create_Object;
   begin
      if C.Has_Row then
         --  Cursor can contain more than one row, so we start by building the
         --  main JSON object from the first row, so we don't repeat the JSON
         --  building code for the same data over and over.
         DB_Columns := Create_Object;

         J := GNATCOLL.JSON.Read (To_String (C.Element.JSON), "json.error");

         DB_Columns.Set_Field (TS (C.Element.Ce_Id.Name),
                               C.Element.Ce_Id.Value);

         DB_Columns.Set_Field (TS (C.Element.Ce_Name.Name),
                               C.Element.Ce_Name.Value);

         DB_Columns.Set_Field (TS (C.Element.Is_Human.Name),
                               C.Element.Is_Human.Value);

         if C.Element.Is_Human.Value then
            J.Set_Field ("type", "human");
         else
            J.Set_Field ("type", "function");
         end if;

         J.Set_Field ("db_columns", DB_Columns);

         while C.Has_Row loop
            if To_String (C.Element.Attr_JSON) /= "" then
               Attr_JSON := Create_Object;
               Attr_DB_Columns := Create_Object;

               Attr_JSON := GNATCOLL.JSON.Read
                 (To_String (C.Element.Attr_JSON),
                  "attr.json.error");

               Attr_DB_Columns.Set_Field (TS (C.Element.Attr_Org_Id.Name),
                                          C.Element.Attr_Org_Id.Value);

               Attr_DB_Columns.Set_Field (TS (C.Element.Attr_Ce_Id.Name),
                                          C.Element.Attr_Ce_Id.Value);

               Attr_JSON.Set_Field ("db_columns", Attr_DB_Columns);

               Append (Attr_Array, Attr_JSON);
            end if;

            C.Next;
         end loop;

         J.Set_Field ("attributes", Attr_Array);
      end if;

      Value := To_JSON_String (J.Write);
   end Contact_Full;

   --------------------
   --  Org_Contacts  --
   --------------------

   procedure Org_Contacts
     (C     : in out Storage.Queries.Org_Contacts_Cursor;
      Value : in out Common.JSON_String)
   is
      use Common;
      use GNATCOLL.JSON;
      use Yolk.Utilities;

      Contact_Array : JSON_Array;
      DB_Columns    : JSON_Value;
      DB_JSON       : JSON_Value;
      J             : constant JSON_Value := Create_Object;
   begin
      while C.Has_Row loop
         DB_Columns := Create_Object;
         DB_JSON    := Create_Object;

         DB_JSON := GNATCOLL.JSON.Read (To_String (C.Element.JSON),
                                        "db_json.json.error");

         DB_Columns.Set_Field (TS (C.Element.Ce_Id.Name),
                               C.Element.Ce_Id.Value);

         DB_Columns.Set_Field (TS (C.Element.Ce_Name.Name),
                               C.Element.Ce_Name.Value);

         DB_Columns.Set_Field (TS (C.Element.Is_Human.Name),
                               C.Element.Is_Human.Value);

         if C.Element.Is_Human.Value then
            DB_JSON.Set_Field ("type", "human");
         else
            DB_JSON.Set_Field ("type", "function");
         end if;

         DB_JSON.Set_Field ("db_columns", DB_Columns);

         Append (Contact_Array, DB_JSON);

         C.Next;
      end loop;

      J.Set_Field ("contacts", Contact_Array);

      Value := To_JSON_String (J.Write);
   end Org_Contacts;

   -------------------------------
   --  Org_Contacts_Attributes  --
   -------------------------------

   procedure Org_Contacts_Attributes
     (C     : in out Storage.Queries.Org_Contacts_Attributes_Cursor;
      Value : in out Common.JSON_String)
   is
      use Common;
      use GNATCOLL.JSON;
      use Yolk.Utilities;

      Attr_Array : JSON_Array;
      DB_Columns : JSON_Value;
      DB_JSON    : JSON_Value;
      J          : constant JSON_Value := Create_Object;
   begin
      while C.Has_Row loop
         DB_Columns := Create_Object;
         DB_JSON    := Create_Object;

         DB_JSON := GNATCOLL.JSON.Read (To_String (C.Element.JSON),
                                        "db_json.json.error");

         DB_Columns.Set_Field (TS (C.Element.Ce_Id.Name),
                               C.Element.Ce_Id.Value);

         DB_Columns.Set_Field (TS (C.Element.Org_Id.Name),
                               C.Element.Org_Id.Value);

         DB_JSON.Set_Field ("db_columns", DB_Columns);

         Append (Attr_Array, DB_JSON);

         C.Next;
      end loop;

      J.Set_Field ("attributes", Attr_Array);

      Value := To_JSON_String (J.Write);
   end Org_Contacts_Attributes;

   -------------------------
   --  Org_Contacts_Full  --
   -------------------------

   procedure Org_Contacts_Full
     (C     : in out Storage.Queries.Org_Contacts_Full_Cursor;
      Value : in out Common.JSON_String)
   is
      use Common;
      use GNATCOLL.JSON;
      use Yolk.Utilities;

      Attr_DB_Columns : JSON_Value;
      Attr_JSON       : JSON_Value;
      Contact_Array   : JSON_Array;
      Contact_JSON    : JSON_Value;
      DB_Columns      : JSON_Value;
      J               : constant JSON_Value := Create_Object;
   begin
      while C.Has_Row loop
         Contact_JSON := Create_Object;
         DB_Columns := Create_Object;

         Contact_JSON := GNATCOLL.JSON.Read (To_String (C.Element.JSON),
                                             "contact_json.json.error");

         DB_Columns.Set_Field (TS (C.Element.Ce_Id.Name),
                               C.Element.Ce_Id.Value);

         DB_Columns.Set_Field (TS (C.Element.Ce_Name.Name),
                               TS (C.Element.Ce_Name.Value));

         DB_Columns.Set_Field (TS (C.Element.Is_Human.Name),
                               C.Element.Is_Human.Value);

         if C.Element.Is_Human.Value then
            Contact_JSON.Set_Field ("type", "human");
         else
            Contact_JSON.Set_Field ("type", "function");
         end if;

         Contact_JSON.Set_Field ("db_columns", DB_Columns);

         Attr_JSON := Create_Object;
         Attr_DB_Columns := Create_Object;

         if To_String (C.Element.Attr_JSON) /= "" then
            Attr_JSON := GNATCOLL.JSON.Read (To_String (C.Element.Attr_JSON),
                                             "attr.json.error");

            Attr_DB_Columns.Set_Field (TS (C.Element.Attr_Org_Id.Name),
                                       C.Element.Attr_Org_Id.Value);

            Attr_DB_Columns.Set_Field (TS (C.Element.Attr_Ce_Id.Name),
                                       C.Element.Attr_Ce_Id.Value);

            Attr_JSON.Set_Field ("db_columns", Attr_DB_Columns);
         end if;

         Contact_JSON.Set_Field ("attributes", Attr_JSON);

         Append (Contact_Array, Contact_JSON);

         C.Next;
      end loop;

      J.Set_Field ("contacts", Contact_Array);

      Value := To_JSON_String (J.Write);
   end Org_Contacts_Full;

   --------------------
   --  Organization  --
   --------------------

   procedure Organization
     (C     : in     Storage.Queries.Organization_Cursor;
      Value : in out Common.JSON_String)
   is
      use Common;
      use GNATCOLL.JSON;
      use Yolk.Utilities;

      DB_Columns : JSON_Value;
      J          : JSON_Value := Create_Object;
   begin
      if C.Has_Row then
         DB_Columns := Create_Object;

         J := GNATCOLL.JSON.Read (To_String (C.Element.JSON), "json.error");

         DB_Columns.Set_Field (TS (C.Element.Org_Id.Name),
                               C.Element.Org_Id.Value);

         DB_Columns.Set_Field (TS (C.Element.Org_Name.Name),
                               TS (C.Element.Org_Name.Value));

         DB_Columns.Set_Field (TS (C.Element.Identifier.Name),
                               TS (C.Element.Identifier.Value));

         J.Set_Field ("db_columns", DB_Columns);
      end if;

      Value := To_JSON_String (J.Write);
   end Organization;

end JSONIFY;
