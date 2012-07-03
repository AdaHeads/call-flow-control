-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                            Contact_Attributes                             --
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

with AWS.Status;
with Database;
with GNATCOLL.JSON;
with My_Configuration;
with Response;
with Storage;
with Yolk.Cache.String_Keys;
with Yolk.Utilities;

package body Contact_Attributes is

   package My renames My_Configuration;

   package Cache is new Yolk.Cache.String_Keys
     (Element_Type      => Common.JSON_String,
      Cleanup_Size      => My.Config.Get (My.Cache_Size_Contact) + 1,
      Cleanup_On_Write  => True,
      Max_Element_Age   => My.Config.Get (My.Cache_Max_Element_Age),
      Reserved_Capacity => My.Config.Get (My.Cache_Size_Contact));
   --  Cache for individual contact_attributes JSON objects.

   function Contact_Attributes_Query
     return GNATCOLL.SQL.Exec.Prepared_Statement
   with inline;
   --  Return the contact_attributes entity SQL query.

   function Contact_Attributes_Query_Parameters
     (Request : in AWS.Status.Data)
      return GNATCOLL.SQL.Exec.SQL_Parameters
   with inline;
   --  Generate the SQL parameters used to fetch the atttributes for a single
   --  contact entity in the database. These parameters are used by the
   --  prepared statement from Contact_Attributes_Query.

   procedure Create_Contact_Attributes_JSON
     (Cursor : in     GNATCOLL.SQL.Exec.Forward_Cursor'Class;
      Value  : in out Common.JSON_String)
   with inline;
   --  If Cursor contains one or more contact entity attributes, then Value is
   --  a contact entity attributes JSON.
   --  If Cursor is empty, then Value is an empty JSON string ({}).

   package Contact_Attributes_Store is new Storage.Generic_Read
     (Cursor           => Contact_Attributes_Cursor,
      Query            => Contact_Attributes_Query,
      JSONIFY          => Create_Contact_Attributes_JSON,
      Write_To_Cache   => Cache.Write,
      Query_Parameters => Contact_Attributes_Query_Parameters);
   --  Find the attributes for a contact entity and turn them into a JSON
   --  string, based on the given Query and Query_Parameters.

   package Generate_Read_Response is new Response.Generic_Read
     (Check_Request_Parameters => Response.Check_Ce_Id_Parameter,
      Get_Key                  => Response.Get_Ce_Id_Key,
      Read_From_Cache          => Cache.Read,
      Store                    => Contact_Attributes_Store);
   --  Generate a contact response.

   --------------------------------
   --  Contact_Attributes_Query  --
   --------------------------------

   function Contact_Attributes_Query
     return GNATCOLL.SQL.Exec.Prepared_Statement
   is
      use Database;
      use GNATCOLL.SQL;
      use GNATCOLL.SQL.Exec;

      Get_Contact_Attributes : constant SQL_Query
        := SQL_Select (Fields =>
                         Contactentity_Attributes.Json &  --  0
                         Contactentity_Attributes.Ce_Id & --  1
                         Contactentity_Attributes.Org_Id, --  2
                       Where  =>
                         Contactentity_Attributes.Ce_Id = (Integer_Param (1)));

      Prepared_Get_Contact_Attributes : constant Prepared_Statement
        := Prepare (Query         => Get_Contact_Attributes,
                    Auto_Complete => True,
                    On_Server     => True,
                    Name          => "get_contact_attributes");
   begin
      return Prepared_Get_Contact_Attributes;
   end Contact_Attributes_Query;

   -------------------------------------------
   --  Contact_Attributes_Query_Parameters  --
   -------------------------------------------

   function Contact_Attributes_Query_Parameters
     (Request : in AWS.Status.Data)
      return GNATCOLL.SQL.Exec.SQL_Parameters
   is
      use GNATCOLL.SQL.Exec;
   begin
      return (1 => +Natural'Value (Response.Get_Ce_Id_Key (Request)));
   end Contact_Attributes_Query_Parameters;

   --------------------------------------
   --  Create_Attributes_Contact_JSON  --
   --------------------------------------

   procedure Create_Contact_Attributes_JSON
     (Cursor : in     GNATCOLL.SQL.Exec.Forward_Cursor'Class;
      Value  : in out Common.JSON_String)
   is
      use Common;
      use GNATCOLL.JSON;
      use Yolk.Utilities;

      Attr_Array : JSON_Array;
      C          : Contact_Attributes_Cursor :=
                     Contact_Attributes_Cursor (Cursor);
      DB_Columns : JSON_Value;
      DB_JSON    : JSON_Value;
      J          : constant JSON_Value := Create_Object;
   begin
      while C.Has_Row loop
         DB_Columns := Create_Object;
         DB_JSON    := Create_Object;

         DB_JSON := GNATCOLL.JSON.Read (To_String (C.Element.JSON),
                                        "db_json.json.error");

         DB_Columns.Set_Field (TS (C.Element.Ce_Id_Column_Name),
                               C.Element.Ce_Id);

         DB_Columns.Set_Field (TS (C.Element.Org_Id_Column_Name),
                               C.Element.Org_Id);

         DB_JSON.Set_Field ("db_columns", DB_Columns);

         Append (Attr_Array, DB_JSON);

         C.Next;
      end loop;

      J.Set_Field ("attributes", Attr_Array);

      Value := To_JSON_String (J.Write);
   end Create_Contact_Attributes_JSON;

   ---------------
   --  Element  --
   ---------------

   function Element
     (C : in Contact_Attributes_Cursor)
      return Contact_Attributes_Row
   is
      use Common;
      use Yolk.Utilities;
   begin
      return Contact_Attributes_Row'
        (JSON                 => To_JSON_String (C.Value (0)),
         Ce_Id                => C.Integer_Value (1, Default => 0),
         Ce_Id_Column_Name    => TUS (C.Field_Name (1)),
         Org_Id               => C.Integer_Value (2, Default => 0),
         Org_Id_Column_Name   => TUS (C.Value (2)));
   end Element;

   ---------------------
   --  Read_Callback  --
   ---------------------

   function Read_Callback
     return AWS.Dispatchers.Callback.Handler
   is
   begin
      return AWS.Dispatchers.Callback.Create
        (Contact_Attributes.Generate_Read_Response.Get'Access);
   end Read_Callback;

end Contact_Attributes;
