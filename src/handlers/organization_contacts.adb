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

      Contacts_Array : JSON_Array;
      Contact_JSON   : JSON_Value;
      JSON           : JSON_Value;
   begin
      JSON := Create_Object;

      while C.Has_Row loop
         Contact_JSON := Create_Object;

         Contact_JSON.Set_Field (TS (C.Element.Ce_Id_Column_Name),
                                 C.Element.Ce_Id);

         Contact_JSON.Set_Field (TS (C.Element.Ce_Name_Column_Name),
                                 C.Element.Ce_Name);

         Contact_JSON.Set_Field (TS (C.Element.Is_Human_Column_Name),
                                 C.Element.Is_Human);

         Append (Contacts_Array, Contact_JSON);

         C.Next;
      end loop;

      JSON.Set_Field ("contacts", Contacts_Array);

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
      return Row'(Ce_Id                => C.Integer_Value (0, Default => 0),
                  Ce_Id_Column_Name    => TUS (C.Field_Name (0)),
                  Ce_Name              => TUS (C.Value (1)),
                  Ce_Name_Column_Name  => TUS (C.Field_Name (1)),
                  Is_Human             => C.Boolean_Value (2),
                  Is_Human_Column_Name => TUS (C.Field_Name (2)));
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
                         DB.Contactentity.Ce_Id &   --  0
                         DB.Contactentity.Ce_Name & --  1
                         DB.Contactentity.Is_Human, --  2
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
