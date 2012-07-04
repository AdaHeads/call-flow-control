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

      DB_Columns : JSON_Value;
      J          : JSON_Value := Create_Object;
   begin
      if C.Has_Row then
         DB_Columns := Create_Object;

         J := GNATCOLL.JSON.Read (To_String (C.Element.JSON), "json.error");

         DB_Columns.Set_Field (TS (C.Element.Org_Id_Column_Name),
                               C.Element.Org_Id);

         DB_Columns.Set_Field (TS (C.Element.Org_Name_Column_Name),
                               TS (C.Element.Org_Name));

         DB_Columns.Set_Field (TS (C.Element.Identifier_Column_Name),
                               TS (C.Element.Identifier));

         J.Set_Field ("db_columns", DB_Columns);
      end if;

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
      return Row'(JSON                   => To_JSON_String (C.Value (0)),
                  Org_Id                 => C.Integer_Value (1, Default => 0),
                  Org_Id_Column_Name     => TUS (C.Field_Name (1)),
                  Org_Name               => TUS (C.Value (2)),
                  Org_Name_Column_Name   => TUS (C.Field_Name (2)),
                  Identifier             => TUS (C.Value (3)),
                  Identifier_Column_Name => TUS (C.Field_Name (3)));
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

      Get_Organization : constant SQL_Query
        :=  SQL_Select (Fields =>
                          DB.Organization.Json &      --  0
                          DB.Organization.Org_Id &    --  1
                          DB.Organization.Org_Name &  --  2
                          DB.Organization.Identifier, --  3
                        Where  => DB.Organization.Org_Id = Integer_Param (1));

      Prepared_Get_Organization : constant Prepared_Statement
        := Prepare (Query         => Get_Organization,
                    Auto_Complete => True,
                    On_Server     => True,
                    Name          => "get_organization");
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
