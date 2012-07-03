-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                 Contact                                   --
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

package body Contact is

   package My renames My_Configuration;

   package Cache is new Yolk.Cache.String_Keys
     (Element_Type      => Common.JSON_String,
      Cleanup_Size      => My.Config.Get (My.Cache_Size_Contact) + 1,
      Cleanup_On_Write  => True,
      Max_Element_Age   => My.Config.Get (My.Cache_Max_Element_Age),
      Reserved_Capacity => My.Config.Get (My.Cache_Size_Contact));
   --  Cache for individual contact JSON objects.

   function Contact_Query
     return GNATCOLL.SQL.Exec.Prepared_Statement
   with inline;
   --  Return the contact entity SQL query.

   function Contact_Query_Parameters
     (Request : in AWS.Status.Data)
      return GNATCOLL.SQL.Exec.SQL_Parameters
   with inline;
   --  Generate the SQL parameters used to single out a contact entity in the
   --  database. These parameters are used by the prepared statement from
   --  Contact_Query.

   procedure Create_Contact_JSON
     (Cursor : in     GNATCOLL.SQL.Exec.Forward_Cursor'Class;
      Value  : in out Common.JSON_String)
   with inline;
   --  If Cursor contains a contact entity, then Value is a contact entity
   --  JSON.
   --  If Cursor is empty, then Value is an empty JSON string ({}).

   package Contact_Store is new Storage.Generic_Read
     (Cursor           => Contact_Cursor,
      Query            => Contact_Query,
      JSONIFY          => Create_Contact_JSON,
      Write_To_Cache   => Cache.Write,
      Query_Parameters => Contact_Query_Parameters);
   --  Find a contact entity and turn it into a JSON string, based on the given
   --  Query and Query_Parameters.

   package Generate_Read_Response is new Response.Generic_Read
     (Check_Request_Parameters => Response.Check_Ce_Id_Parameter,
      Get_Key                  => Response.Get_Ce_Id_Key,
      Read_From_Cache          => Cache.Read,
      Store                    => Contact_Store);
   --  Generate a contact response.

   ---------------------
   --  Contact_Query  --
   ---------------------

   function Contact_Query
     return GNATCOLL.SQL.Exec.Prepared_Statement
   is
      use Database;
      use GNATCOLL.SQL;
      use GNATCOLL.SQL.Exec;

      Get_Contact : constant SQL_Query
        := SQL_Select (Fields =>
                         Contactentity.Json &    --  0
                         Contactentity.Ce_Id &   --  1
                         Contactentity.Ce_Name & --  2
                         Contactentity.Is_Human, --  3
                       Where  =>
                         Contactentity.Ce_Id = Integer_Param (1));

      Prepared_Get_Contact : constant Prepared_Statement
        := Prepare (Query         => Get_Contact,
                    Auto_Complete => True,
                    On_Server     => True,
                    Name          => "get_contact");
   begin
      return Prepared_Get_Contact;
   end Contact_Query;

   --------------------------------
   --  Contact_Query_Parameters  --
   --------------------------------

   function Contact_Query_Parameters
     (Request : in AWS.Status.Data)
      return GNATCOLL.SQL.Exec.SQL_Parameters
   is
      use GNATCOLL.SQL.Exec;
   begin
      return (1 => +Natural'Value (Response.Get_Ce_Id_Key (Request)));
   end Contact_Query_Parameters;

   ---------------------------
   --  Create_Contact_JSON  --
   ---------------------------

   procedure Create_Contact_JSON
     (Cursor : in     GNATCOLL.SQL.Exec.Forward_Cursor'Class;
      Value  : in out Common.JSON_String)
   is
      use Common;
      use GNATCOLL.JSON;
      use Yolk.Utilities;

      C          : constant Contact_Cursor := Contact_Cursor (Cursor);
      DB_Columns : JSON_Value;
      J          : JSON_Value := Create_Object;
   begin
      if C.Has_Row then
         DB_Columns := Create_Object;

         J := GNATCOLL.JSON.Read (To_String (C.Element.JSON), "json.error");

         DB_Columns.Set_Field (TS (C.Element.Ce_Id_Column_Name),
                               C.Element.Ce_Id);

         DB_Columns.Set_Field (TS (C.Element.Ce_Name_Column_Name),
                               C.Element.Ce_Name);

         DB_Columns.Set_Field (TS (C.Element.Is_Human_Column_Name),
                               C.Element.Is_Human);

         if C.Element.Is_Human then
            J.Set_Field ("type", "human");
         else
            J.Set_Field ("type", "function");
         end if;

         J.Set_Field ("db_columns", DB_Columns);
      end if;

      Value := To_JSON_String (J.Write);
   end Create_Contact_JSON;

   ---------------
   --  Element  --
   ---------------

   function Element
     (C : in Contact_Cursor)
      return Contact_Row
   is
      use Common;
      use Yolk.Utilities;
   begin
      return Contact_Row'
        (JSON                 => To_JSON_String (C.Value (0)),
         Ce_Id                => C.Integer_Value (1, Default => 0),
         Ce_Id_Column_Name    => TUS (C.Field_Name (1)),
         Ce_Name              => TUS (C.Field_Name (2)),
         Ce_Name_Column_Name  => TUS (C.Value (2)),
         Is_Human             => C.Boolean_Value (3),
         Is_Human_Column_Name => TUS (C.Field_Name (3)));
   end Element;

   ---------------------
   --  Read_Callback  --
   ---------------------

   function Read_Callback
     return AWS.Dispatchers.Callback.Handler
   is
   begin
      return AWS.Dispatchers.Callback.Create
        (Contact.Generate_Read_Response.Get'Access);
   end Read_Callback;

end Contact;
