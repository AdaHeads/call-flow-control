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

with AWS.Status;
with Database;
with System_Message.Error;

package body Organization is

   ---------------------
   --  Bad_Org_Id_Key  --
   ---------------------

   procedure Bad_Org_Id_Key
     (Response_Object :    out Response.Object;
      Message         : in     String)
   is
      use System_Message;
   begin
      Error.Bad_Org_Id_Key.Notify (Message, Response_Object);
   end Bad_Org_Id_Key;

   ----------------
   --  Callback  --
   ----------------

   function Callback
     return AWS.Dispatchers.Callback.Handler
   is
   begin
      return AWS.Dispatchers.Callback.Create (JSON_Response'Access);
   end Callback;

   -------------------
   --  Create_JSON  --
   -------------------

   function Create_JSON
     (C : in out Cursor)
      return Common.JSON_String
   is
      use Common;

      procedure Build_Contact_JSON;
      --  Build an attribute node for a contact entity.

      A_Row          : Row;
      Contacts_Array : JSON_Array;
      Contact_JSON   : JSON_Value;
      JSON           : JSON_Value;

      --------------------------
      --  Build_Contact_JSON  --
      --------------------------

      procedure Build_Contact_JSON
      is
      begin
         Contact_JSON := Create_Object;

         Contact_JSON.Set_Field (To_String (A_Row.Ce_Id_Column_Name),
                                 A_Row.Ce_Id);

         Contact_JSON.Set_Field (To_String (A_Row.Ce_Name_Column_Name),
                                 To_String (A_Row.Ce_Name));

         Contact_JSON.Set_Field (To_String (A_Row.Is_Human_Column_Name),
                                 A_Row.Is_Human);

         if A_Row.Attr_JSON /= JSON_Null then
            Contact_JSON.Set_Field ("attributes", A_Row.Attr_JSON);
         end if;

         Append (Contacts_Array, Contact_JSON);
      end Build_Contact_JSON;
   begin
      JSON := Create_Object;

      if C.Has_Row then
         A_Row := C.Element;

         JSON := A_Row.Org_JSON;

         JSON.Set_Field (To_String (A_Row.Org_Id_Column_Name),
                         A_Row.Org_Id);

         JSON.Set_Field (To_String (A_Row.Org_Name_Column_Name),
                         A_Row.Org_Name);

         JSON.Set_Field (To_String (A_Row.Identifier_Column_Name),
                         A_Row.Identifier);

         Build_Contact_JSON;

         C.Next;

         while C.Has_Row loop
            --  If we have more than one row, then we have more than one
            --  contact entity in the organization.
            A_Row := C.Element;
            Build_Contact_JSON;
            C.Next;
         end loop;

         JSON.Set_Field ("contact", Contacts_Array);
      end if;

      return To_JSON_String (JSON.Write);
   end Create_JSON;

   ---------------
   --  Element  --
   ---------------

   function Element
     (C : in Cursor)
      return Row
   is
      use Common;
   begin
      return Row'(Org_JSON               => C.Json_Object_Value (0),
                  Org_Id                 => C.Integer_Value (1, Default => 0),
                  Org_Id_Column_Name     => U ("organization_id"),
                  Org_Name               => U (C.Value (2)),
                  Org_Name_Column_Name   => U (C.Field_Name (2)),
                  Identifier             => U (C.Value (3)),
                  Identifier_Column_Name => U (C.Field_Name (3)),
                  Ce_Id                  => C.Integer_Value (4, Default => 0),
                  Ce_Id_Column_Name      => U ("contactentity_id"),
                  Ce_Name                => U (C.Value (5)),
                  Ce_Name_Column_Name    => U (C.Field_Name (5)),
                  Is_Human               => C.Boolean_Value (6),
                  Is_Human_Column_Name   => U (C.Field_Name (6)),
                  Attr_JSON              => C.Json_Object_Value (7));
   end Element;

   ----------------------
   --  Get_Org_Id_Key  --
   ----------------------

   function Get_Org_Id_Key
     (Response_Object : in Response.Object)
      return Natural
   is
      use AWS.Status;
   begin
      return Natural'Value
        (Parameters (Response_Object.Get_Request).Get ("org_id"));
   end Get_Org_Id_Key;

   ----------------------
   --  Prepared_Query  --
   ----------------------

   function Prepared_Query
     return GNATCOLL.SQL.Exec.Prepared_Statement
   is
      package DB renames Database;

      use GNATCOLL.SQL;
      use GNATCOLL.SQL.Exec;

      Organization_Contacts_Join : constant SQL_Left_Join_Table
        := Join (Table1 => DB.Organization,
                 Table2 => DB.Organization_Contacts,
                 On     =>
                   DB.Organization_Contacts.FK (DB.Organization));

      Contacts_Join : constant SQL_Left_Join_Table
        := Join (Table1 => Organization_Contacts_Join,
                 Table2 => DB.Contact,
                 On     =>
                   DB.Organization_Contacts.FK (DB.Contact));

      Attributes_Left_Join : constant SQL_Left_Join_Table
        := Left_Join (Full    => Contacts_Join,
                      Partial => DB.Contact_Attributes,
                      On      =>
                        DB.Contact_Attributes.FK (DB.Contact));

      Org_Full : constant SQL_Query
        := SQL_Select (Fields =>
                         DB.Organization.Json &             --  0
                         DB.Organization.Id &               --  1
                         DB.Organization.Full_Name &             --  2
                         DB.Organization.Identifier &       --  3
                         DB.Contact.Id &              --  4
                         DB.Contact.Full_Name &       --  5
                         DB.Contact.Is_Human &        --  6
                         DB.Contact_Attributes.Json,  --  7
                       From   => Attributes_Left_Join,
                       Where  =>
                         DB.Organization.Id = Integer_Param (1)
                       and
                         (DB.Contact_Attributes.Organization_Id =
                            Integer_Param (1)
                          or
                            Is_Null
                              (DB.Contact_Attributes.Organization_Id)));

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
     (Response_Object : in Response.Object)
      return GNATCOLL.SQL.Exec.SQL_Parameters
   is
      use GNATCOLL.SQL.Exec;
   begin
      return (1 => +Get_Org_Id_Key (Response_Object));
   end Query_Parameters;

end Organization;
