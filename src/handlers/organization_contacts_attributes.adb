-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                      Organization_Contacts_Attributes                     --
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

package body Organization_Contacts_Attributes is

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

      Attr_Array : JSON_Array;
      Attr_JSON  : JSON_Value;
      JSON       : JSON_Value;
   begin
      JSON := Create_Object;

      while C.Has_Row loop
         Attr_JSON    := Create_Object;

         Attr_JSON := GNATCOLL.JSON.Read
           (To_String (C.Element.JSON),
            "organization_contacts_attributes.json.error");

         Attr_JSON.Set_Field (TS (C.Element.Ce_Id_Column_Name),
                              C.Element.Ce_Id);

         Attr_JSON.Set_Field (TS (C.Element.Org_Id_Column_Name),
                              C.Element.Org_Id);

         Append (Attr_Array, Attr_JSON);

         C.Next;
      end loop;

      JSON.Set_Field ("attributes", Attr_Array);

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
      return Row'(JSON               => To_JSON_String (C.Value (0)),
                  Ce_Id              => C.Integer_Value (1, Default => 0),
                  Ce_Id_Column_Name  => TUS (C.Field_Name (1)),
                  Org_Id             => C.Integer_Value (2, Default => 0),
                  Org_Id_Column_Name => TUS (C.Field_Name (2)));
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

      Get_Org_Contacts_Attributes : constant SQL_Query
        := SQL_Select (Fields =>
                         DB.Contactentity_Attributes.Json &   --  0
                         DB.Contactentity_Attributes.Org_Id & --  1
                         DB.Contactentity_Attributes.Ce_Id,   --  2
                       Where  =>
                         DB.Contactentity_Attributes.Org_Id =
                           Integer_Param (1));

      Prepared_Get_Org_Contacts_Attributes : constant Prepared_Statement
        := Prepare (Query         => Get_Org_Contacts_Attributes,
                    Auto_Complete => True,
                    On_Server     => True,
                    Name          => "get_org_contacts_attributes");
   begin
      return Prepared_Get_Org_Contacts_Attributes;
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

end Organization_Contacts_Attributes;
