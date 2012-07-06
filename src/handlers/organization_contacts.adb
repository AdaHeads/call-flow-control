-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                           Organization_Contacts                           --
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

package body Organization_Contacts is

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

      Contact_Array : JSON_Array;
      DB_Columns    : JSON_Value;
      DB_JSON       : JSON_Value;
      J             : constant JSON_Value := Create_Object;
   begin
      while C.Has_Row loop
         DB_Columns := Create_Object;
         DB_JSON    := Create_Object;

         DB_JSON := GNATCOLL.JSON.Read (To_String (C.Element.JSON),
                                        "organization_contacts.json.error");

         DB_Columns.Set_Field (TS (C.Element.Ce_Id_Column_Name),
                               C.Element.Ce_Id);

         DB_Columns.Set_Field (TS (C.Element.Ce_Name_Column_Name),
                               C.Element.Ce_Name);

         DB_Columns.Set_Field (TS (C.Element.Is_Human_Column_Name),
                               C.Element.Is_Human);

         if C.Element.Is_Human then
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
      return Row'(JSON                 => To_JSON_String (C.Value (0)),
                  Ce_Id                => C.Integer_Value (1, Default => 0),
                  Ce_Id_Column_Name    => TUS (C.Field_Name (1)),
                  Ce_Name              => TUS (C.Value (2)),
                  Ce_Name_Column_Name  => TUS (C.Field_Name (2)),
                  Is_Human             => C.Boolean_Value (3),
                  Is_Human_Column_Name => TUS (C.Field_Name (3)));
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

      Get_Org_Contacts_Join : constant SQL_Left_Join_Table
        := Join (Table1 => DB.Contactentity,
                 Table2 => DB.Organization_Contactentities,
                 On     =>
                   DB.Contactentity.Ce_Id =
                     DB.Organization_Contactentities.Ce_Id);

      Get_Org_Contacts : constant SQL_Query
        := SQL_Select (Fields =>
                         DB.Contactentity.Json &    --  0
                         DB.Contactentity.Ce_Id &   --  1
                         DB.Contactentity.Ce_Name & --  2
                         DB.Contactentity.Is_Human, --  3
                       From   => Get_Org_Contacts_Join,
                       Where  =>
                         DB.Organization_Contactentities.Org_Id =
                           Integer_Param (1));

      Prepared_Get_Org_Contacts : constant Prepared_Statement
        := Prepare (Query         => Get_Org_Contacts,
                    Auto_Complete => True,
                    On_Server     => True,
                    Name          => "get_org_contacts");
   begin
      return Prepared_Get_Org_Contacts;
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

end Organization_Contacts;
