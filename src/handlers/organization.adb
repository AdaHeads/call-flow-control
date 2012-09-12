-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                              Organization                                 --
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

with Database;
with GNATCOLL.JSON;
with Yolk.Utilities;

package body Organization is

   ----------------
   --  Callback  --
   ----------------

   function Callback
     return AWS.Dispatchers.Callback.Handler
   is
   begin
      return AWS.Dispatchers.Callback.Create (JSON_Response.Generate'Access);
   end Callback;

   -------------------
   --  Create_JSON  --
   -------------------

   procedure Create_JSON
     (C     : in out Cursor;
      Value : in out Common.JSON_String)
   is
      use Common;
      use GNATCOLL.JSON;
      use Yolk.Utilities;

      Attr_JSON      : JSON_Value;
      Contacts_Array : JSON_Array;
      Contact_JSON   : JSON_Value;
      JSON           : JSON_Value;
   begin
      JSON := Create_Object;

      if C.Has_Row then
         --  C can contain more than one row, so we start by building the main
         --  JSON object from the first row, so we don't repeat the JSON
         --  building code for the same data over and over.

         JSON := GNATCOLL.JSON.Read (To_String (C.Element.Org_JSON),
                                     "org.json.error");

         JSON.Set_Field (TS (C.Element.Org_Id_Column_Name),
                         C.Element.Org_Id);

         JSON.Set_Field (TS (C.Element.Org_Name_Column_Name),
                         C.Element.Org_Name);

         JSON.Set_Field (TS (C.Element.Identifier_Column_Name),
                         C.Element.Identifier);

         while C.Has_Row loop
            Contact_JSON := Create_Object;

            Contact_JSON.Set_Field (TS (C.Element.Ce_Id_Column_Name),
                                    C.Element.Ce_Id);

            Contact_JSON.Set_Field (TS (C.Element.Ce_Name_Column_Name),
                                    TS (C.Element.Ce_Name));

            Contact_JSON.Set_Field (TS (C.Element.Is_Human_Column_Name),
                                    C.Element.Is_Human);

            Attr_JSON := Create_Object;

            if To_String (C.Element.Attr_JSON) /= "" then
               Attr_JSON := GNATCOLL.JSON.Read
                 (To_String (C.Element.Attr_JSON),
                  "organization_attribute.json.error");
            end if;

            Contact_JSON.Set_Field ("attribute", Attr_JSON);

            Append (Contacts_Array, Contact_JSON);

            C.Next;
         end loop;

         JSON.Set_Field ("contact", Contacts_Array);
      end if;

      Value := To_JSON_String (JSON.Write);
   end Create_JSON;

   ---------------
   --  Element  --
   ---------------

   function Element
     (C : in Cursor)
      return Row
   is
      use Common;
      use Yolk.Utilities;
   begin
      return Row'(Org_JSON                => To_JSON_String (C.Value (0)),
                  Org_Id                  => C.Integer_Value (1, Default => 0),
                  Org_Id_Column_Name      => TUS (C.Field_Name (1)),
                  Org_Name                => TUS (C.Value (2)),
                  Org_Name_Column_Name    => TUS (C.Field_Name (2)),
                  Identifier              => TUS (C.Value (3)),
                  Identifier_Column_Name  => TUS (C.Field_Name (3)),
                  Ce_Id                   => C.Integer_Value (4, Default => 0),
                  Ce_Id_Column_Name       => TUS (C.Field_Name (4)),
                  Ce_Name                 => TUS (C.Value (5)),
                  Ce_Name_Column_Name     => TUS (C.Field_Name (5)),
                  Is_Human                => C.Boolean_Value (6),
                  Is_Human_Column_Name    => TUS (C.Field_Name (6)),
                  Attr_JSON               => To_JSON_String (C.Value (7)));
   end Element;

   ----------------------
   --  Prepared_Query  --
   ----------------------

   function Prepared_Query
     return GNATCOLL.SQL.Exec.Prepared_Statement
   is
      package DB renames Database;

      use GNATCOLL.SQL;
      use GNATCOLL.SQL.Exec;

      Organization_Contactentities_Join : constant SQL_Left_Join_Table
        := Join (Table1 => DB.Organization,
                 Table2 => DB.Organization_Contactentities,
                 On     =>
                   DB.Organization.Org_Id =
                     DB.Organization_Contactentities.Org_Id);

      Contacts_Join : constant SQL_Left_Join_Table
        := Join (Table1 => Organization_Contactentities_Join,
                 Table2 => DB.Contactentity,
                 On     =>
                   DB.Organization_Contactentities.Ce_Id =
                     DB.Contactentity.Ce_Id);

      Attributes_Left_Join : constant SQL_Left_Join_Table
        := Left_Join (Full    => Contacts_Join,
                      Partial => DB.Contactentity_Attributes,
                      On      =>
                        DB.Contactentity.Ce_Id =
                          DB.Contactentity_Attributes.Ce_Id);

      Org_Full : constant SQL_Query
        := SQL_Select (Fields =>
                         DB.Organization.Json &               --  0
                         DB.Organization.Org_Id &             --  1
                         DB.Organization.Org_Name &           --  2
                         DB.Organization.Identifier &         --  3
                         DB.Contactentity.Ce_Id &             --  4
                         DB.Contactentity.Ce_Name &           --  5
                         DB.Contactentity.Is_Human &          --  6
                         DB.Contactentity_Attributes.Json,    --  7
                       From   => Attributes_Left_Join,
                       Where  =>
                         DB.Organization.Org_Id = Integer_Param (1)
                       and
                         (DB.Contactentity_Attributes.Org_Id =
                                                    Integer_Param (1)
                          or
                            Is_Null (DB.Contactentity_Attributes.Org_Id)));

      Prepared_Get_Organization : constant Prepared_Statement
        := Prepare (Query         => Org_Full,
                    Auto_Complete => True,
                    On_Server     => True,
                    Name          => "organization");
   begin
      return Prepared_Get_Organization;
   end Prepared_Query;

   ------------------------
   --  Query_Parameters  --
   ------------------------

   function Query_Parameters
     (Request : in AWS.Status.Data)
      return GNATCOLL.SQL.Exec.SQL_Parameters
   is
      use GNATCOLL.SQL.Exec;
   begin
      return (1 => +Natural'Value (Response.Get_Org_Id_Key (Request)));
   end Query_Parameters;

end Organization;
