-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                             Storage.Queries                               --
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
with Yolk.Utilities;

package body Storage.Queries is

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

   --------------------------
   --  Contact_Full_Query  --
   --------------------------

   function Contact_Full_Query
     return GNATCOLL.SQL.Exec.Prepared_Statement
   is
      use Database;
      use GNATCOLL.SQL;
      use GNATCOLL.SQL.Exec;

      Get_Contact_Full_Left_Join : constant SQL_Left_Join_Table
        :=  Left_Join (Full    => Contactentity,
                       Partial => Contactentity_Attributes,
                       On      =>
                         Contactentity.Ce_Id = Contactentity_Attributes.Ce_Id);

      Get_Contact_Full : constant SQL_Query
        := SQL_Select (Fields =>
                         Contactentity.Json &              --  0
                         Contactentity.Ce_Id &             --  1
                         Contactentity.Ce_Name &           --  2
                         Contactentity.Is_Human &          --  3
                         Contactentity_Attributes.Json &   --  4
                         Contactentity_Attributes.Org_Id & --  5
                         Contactentity_Attributes.Ce_Id,   --  6
                       From   => Get_Contact_Full_Left_Join,
                       Where  => Contactentity.Ce_Id = Integer_Param (1));

      Prepared_Get_Contact_Full : constant Prepared_Statement
        := Prepare (Query         => Get_Contact_Full,
                    Auto_Complete => True,
                    On_Server     => True,
                    Name          => "get_contact_full");
   begin
      return Prepared_Get_Contact_Full;
   end Contact_Full_Query;

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

   ---------------
   --  Element  --
   ---------------

   function Element
     (C : in Contact_Cursor)
      return Contact_Row
   is
      use Yolk.Utilities;
   begin
      return Contact_Row'
        (JSON     => TUS (C.Value (0)),
         Ce_Id    =>
           Pair_Natural'(Name  => TUS (C.Field_Name (1)),
                         Value => C.Integer_Value (1, Default => 0)),
         Ce_Name  =>
           Pair_String'(Name  => TUS (C.Field_Name (2)),
                        Value => TUS (C.Value (2))),
         Is_Human =>
           Pair_Boolean'(Name  => TUS (C.Field_Name (3)),
                         Value => C.Boolean_Value (3)));
   end Element;

   ---------------
   --  Element  --
   ---------------

   function Element
     (C : in Contact_Attributes_Cursor)
      return Contact_Attributes_Row
   is
      use Yolk.Utilities;
   begin
      return Contact_Attributes_Row'
        (JSON   => TUS (C.Value (0)),
         Ce_Id  =>
           Pair_Natural'(Name  => TUS (C.Field_Name (1)),
                         Value => C.Integer_Value (1, Default => 0)),
         Org_Id =>
           Pair_Natural'(Name  => TUS (C.Field_Name (2)),
                         Value => C.Integer_Value (2, Default => 0)));
   end Element;

   ---------------
   --  Element  --
   ---------------

   function Element
     (C : in Contact_Full_Cursor)
      return Contact_Full_Row
   is
      use Yolk.Utilities;
   begin
      return Contact_Full_Row'
        (JSON        => TUS (C.Value (0)),
         Ce_Id       =>
           Pair_Natural'(Name  => TUS (C.Field_Name (1)),
                         Value => C.Integer_Value (1, Default => 0)),
         Ce_Name     =>
           Pair_String'(Name  => TUS (C.Field_Name (2)),
                        Value => TUS (C.Value (2))),
         Is_Human    =>
           Pair_Boolean'(Name  => TUS (C.Field_Name (3)),
                         Value => C.Boolean_Value (3)),
         Attr_JSON   => TUS (C.Value (4)),
         Attr_Org_Id =>
           Pair_Natural'(Name  => TUS (C.Field_Name (5)),
                         Value => C.Integer_Value (5, Default => 0)),
         Attr_Ce_Id  =>
           Pair_Natural'(Name  => TUS (C.Field_Name (6)),
                         Value => C.Integer_Value (6, Default => 0)));
   end Element;

   ---------------
   --  Element  --
   ---------------

   function Element
     (C : in Organization_Cursor)
      return Organization_Row
   is
      use Yolk.Utilities;
   begin
      return Organization_Row'
        (JSON       => TUS (C.Value (0)),
         Org_Id     =>
           Pair_Natural'(Name  => TUS (C.Field_Name (1)),
                         Value => C.Integer_Value (1, Default => 0)),
         Org_Name   =>
           Pair_String'(Name  => TUS (C.Field_Name (2)),
                        Value => TUS (C.Value (2))),
         Identifier =>
           Pair_String'(Name  => TUS (C.Field_Name (3)),
                        Value => TUS (C.Value (3))));
   end Element;

   ---------------
   --  Element  --
   ---------------

   function Element
     (C : in Org_Contacts_Cursor)
      return Org_Contacts_Row
   is
      use Yolk.Utilities;
   begin
      return Org_Contacts_Row'
        (JSON     => TUS (C.Value (0)),
         Ce_Id    =>
           Pair_Natural'(Name  => TUS (C.Field_Name (1)),
                         Value => C.Integer_Value (1, Default => 0)),
         Ce_Name  =>
           Pair_String'(Name  => TUS (C.Field_Name (2)),
                        Value => TUS (C.Value (2))),
         Is_Human =>
           Pair_Boolean'(Name  => TUS (C.Field_Name (3)),
                         Value => C.Boolean_Value (3)));
   end Element;

   ---------------
   --  Element  --
   ---------------

   function Element
     (C : in Org_Contacts_Attributes_Cursor)
      return Org_Contacts_Attributes_Row
   is
      use Yolk.Utilities;
   begin
      return Org_Contacts_Attributes_Row'
        (JSON   => TUS (C.Value (0)),
         Ce_Id  =>
           Pair_Natural'(Name  => TUS (C.Field_Name (1)),
                         Value => C.Integer_Value (1, Default => 0)),
         Org_Id =>
           Pair_Natural'(Name  => TUS (C.Field_Name (2)),
                         Value => C.Integer_Value (2, Default => 0)));
   end Element;

   ---------------
   --  Element  --
   ---------------

   function Element
     (C : in Org_Contacts_Full_Cursor)
      return Org_Contacts_Full_Row
   is
      use Yolk.Utilities;
   begin
      return Org_Contacts_Full_Row'
        (JSON        => TUS (C.Value (0)),
         Ce_Id       =>
           Pair_Natural'(Name  => TUS (C.Field_Name (1)),
                         Value => C.Integer_Value (1, Default => 0)),
         Ce_Name     =>
           Pair_String'(Name  => TUS (C.Field_Name (2)),
                        Value => TUS (C.Value (2))),
         Is_Human    =>
           Pair_Boolean'(Name  => TUS (C.Field_Name (3)),
                         Value => C.Boolean_Value (3)),
         Attr_JSON   => TUS (C.Value (4)),
         Attr_Org_Id =>
           Pair_Natural'(Name  => TUS (C.Field_Name (5)),
                         Value => C.Integer_Value (5, Default => 0)),
         Attr_Ce_Id  =>
           Pair_Natural'(Name  => TUS (C.Field_Name (6)),
                         Value => C.Integer_Value (6, Default => 0)));
   end Element;

   -------------------------------------
   --  Org_Contacts_Attributes_Query  --
   -------------------------------------

   function Org_Contacts_Attributes_Query
     return GNATCOLL.SQL.Exec.Prepared_Statement
   is
      use Database;
      use GNATCOLL.SQL;
      use GNATCOLL.SQL.Exec;

      Get_Org_Contacts_Attributes : constant SQL_Query
        := SQL_Select (Fields =>
                         Contactentity_Attributes.Json &   --  0
                         Contactentity_Attributes.Org_Id & --  1
                         Contactentity_Attributes.Ce_Id,   --  2
                       Where  =>
                         Contactentity_Attributes.Org_Id = Integer_Param (1));

      Prepared_Get_Org_Contacts_Attributes : constant Prepared_Statement
        := Prepare (Query         => Get_Org_Contacts_Attributes,
                    Auto_Complete => True,
                    On_Server     => True,
                    Name          => "get_org_contacts_attributes");
   begin
      return Prepared_Get_Org_Contacts_Attributes;
   end Org_Contacts_Attributes_Query;

   -------------------------------
   --  Org_Contacts_Full_Query  --
   -------------------------------

   function Org_Contacts_Full_Query
     return GNATCOLL.SQL.Exec.Prepared_Statement
   is
      use Database;
      use GNATCOLL.SQL;
      use GNATCOLL.SQL.Exec;

      Get_Org_Contacts_Full_Join : constant SQL_Left_Join_Table
        := Join (Table1 => Contactentity,
                 Table2 => Organization_Contactentities,
                 On     =>
                   Contactentity.Ce_Id = Organization_Contactentities.Ce_Id);

      Get_Org_Contacts_Full_Left_Join : constant SQL_Left_Join_Table
        := Left_Join (Full    => Get_Org_Contacts_Full_Join,
                      Partial => Contactentity_Attributes,
                      On      =>
                        Contactentity.Ce_Id =  Contactentity_Attributes.Ce_Id);

      Get_Org_Contacts_Full : constant SQL_Query
        := SQL_Select (Fields =>
                         Contactentity.Json &              --  0
                         Contactentity.Ce_Id &             --  1
                         Contactentity.Ce_Name &           --  2
                         Contactentity.Is_Human &          --  3
                         Contactentity_Attributes.Json &   --  4
                         Contactentity_Attributes.Org_Id & --  5
                         Contactentity_Attributes.Ce_Id,   --  6
                       From   => Get_Org_Contacts_Full_Left_Join,
                       Where  =>
                         Organization_Contactentities.Org_Id =
                           Integer_Param (1)
                       and
                         (Contactentity_Attributes.Org_Id = Integer_Param (1)
                          or
                            Is_Null (Contactentity_Attributes.Org_Id)));

      Prepared_Get_Org_Contacts_Full : constant Prepared_Statement
        := Prepare (Query         => Get_Org_Contacts_Full,
                    Auto_Complete => True,
                    On_Server     => True,
                    Name          => "get_org_contacts_full");
   begin
      return Prepared_Get_Org_Contacts_Full;
   end Org_Contacts_Full_Query;

   --------------------------
   --  Org_Contacts_Query  --
   --------------------------

   function Org_Contacts_Query
     return GNATCOLL.SQL.Exec.Prepared_Statement
   is
      use Database;
      use GNATCOLL.SQL;
      use GNATCOLL.SQL.Exec;

      Get_Org_Contacts_Join : constant SQL_Left_Join_Table
        := Join (Table1 => Contactentity,
                 Table2 => Organization_Contactentities,
                 On     =>
                   Contactentity.Ce_Id = Organization_Contactentities.Ce_Id);

      Get_Org_Contacts : constant SQL_Query
        := SQL_Select (Fields =>
                         Contactentity.Json &    --  0
                         Contactentity.Ce_Id &   --  1
                         Contactentity.Ce_Name & --  2
                         Contactentity.Is_Human, --  3
                       From   => Get_Org_Contacts_Join,
                       Where  =>
                         Organization_Contactentities.Org_Id =
                           Integer_Param (1));

      Prepared_Get_Org_Contacts : constant Prepared_Statement
        := Prepare (Query         => Get_Org_Contacts,
                    Auto_Complete => True,
                    On_Server     => True,
                    Name          => "get_org_contacts");
   begin
      return Prepared_Get_Org_Contacts;
   end Org_Contacts_Query;

   --------------------------
   --  Organization_Query  --
   --------------------------

   function Organization_Query
     return GNATCOLL.SQL.Exec.Prepared_Statement
   is
      use Database;
      use GNATCOLL.SQL;
      use GNATCOLL.SQL.Exec;

      Get_Organization : constant SQL_Query
        :=  SQL_Select (Fields =>
                          Organization.Json &      --  0
                          Organization.Org_Id &    --  1
                          Organization.Org_Name &  --  2
                          Organization.Identifier, --  3
                        Where  => Organization.Org_Id = Integer_Param (1));

      Prepared_Get_Organization : constant Prepared_Statement
        := Prepare (Query         => Get_Organization,
                    Auto_Complete => True,
                    On_Server     => True,
                    Name          => "get_organization");
   begin
      return Prepared_Get_Organization;
   end Organization_Query;

end Storage.Queries;
