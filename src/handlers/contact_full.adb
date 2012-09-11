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

with Database;
with GNATCOLL.JSON;
with Yolk.Utilities;

package body Contact_Full is

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

      Attr_Array      : JSON_Array;
      Attr_JSON       : JSON_Value;
      JSON            : JSON_Value;
   begin
      JSON := Create_Object;

      if C.Has_Row then
         --  Cursor can contain more than one row, so we start by building the
         --  main JSON object from the first row, so we don't repeat the JSON
         --  building code for the same data over and over.
         JSON.Set_Field (TS (C.Element.Ce_Id_Column_Name),
                         C.Element.Ce_Id);

         JSON.Set_Field (TS (C.Element.Ce_Name_Column_Name),
                         C.Element.Ce_Name);

         JSON.Set_Field (TS (C.Element.Is_Human_Column_Name),
                         C.Element.Is_Human);

         while C.Has_Row loop
            if To_String (C.Element.Attr_JSON) /= "" then
               Attr_JSON := Create_Object;

               Attr_JSON := GNATCOLL.JSON.Read
                 (To_String (C.Element.Attr_JSON),
                  "attr.json.error");

               Attr_JSON.Set_Field
                 (TS (C.Element.Attr_Org_Id_Column_Name),
                  C.Element.Attr_Org_Id);

               Attr_JSON.Set_Field
                 (TS (C.Element.Attr_Ce_Id_Column_Name),
                  C.Element.Attr_Ce_Id);

               Append (Attr_Array, Attr_JSON);
            end if;

            C.Next;
         end loop;

         JSON.Set_Field ("attributes", Attr_Array);
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
      return Row'(Ce_Id                   => C.Integer_Value (0, Default => 0),
                  Ce_Id_Column_Name       => TUS (C.Field_Name (0)),
                  Ce_Name                 => TUS (C.Value (1)),
                  Ce_Name_Column_Name     => TUS (C.Field_Name (1)),
                  Is_Human                => C.Boolean_Value (2),
                  Is_Human_Column_Name    => TUS (C.Field_Name (2)),
                  Attr_JSON               => To_JSON_String (C.Value (3)),
                  Attr_Org_Id             => C.Integer_Value (4, Default => 0),
                  Attr_Org_Id_Column_Name => TUS (C.Field_Name (4)),
                  Attr_Ce_Id              => C.Integer_Value (5, Default => 0),
                  Attr_Ce_Id_Column_Name  => TUS (C.Field_Name (5)));
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

      Get_Contact_Full_Left_Join : constant SQL_Left_Join_Table
        :=  Left_Join (Full    => DB.Contactentity,
                       Partial => DB.Contactentity_Attributes,
                       On      =>
                         DB.Contactentity.Ce_Id =
                           DB.Contactentity_Attributes.Ce_Id);

      Get_Contact_Full : constant SQL_Query
        := SQL_Select (Fields =>
                         DB.Contactentity.Ce_Id &             --  0
                         DB.Contactentity.Ce_Name &           --  1
                         DB.Contactentity.Is_Human &          --  2
                         DB.Contactentity_Attributes.Json &   --  3
                         DB.Contactentity_Attributes.Org_Id & --  4
                         DB.Contactentity_Attributes.Ce_Id,   --  5
                       From   => Get_Contact_Full_Left_Join,
                       Where  => DB.Contactentity.Ce_Id = Integer_Param (1));

      Prepared_Get_Contact_Full : constant Prepared_Statement
        := Prepare (Query         => Get_Contact_Full,
                    Auto_Complete => True,
                    On_Server     => True,
                    Name          => "get_contact_full");
   begin
      return Prepared_Get_Contact_Full;
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
      return (1 => +Natural'Value (Response.Get_Ce_Id_Key (Request)));
   end Query_Parameters;

end Contact_Full;
