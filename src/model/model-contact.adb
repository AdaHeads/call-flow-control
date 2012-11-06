-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                              Model.Contact                                --
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

with Common;
with Database;

package body Model.Contact is

   ---------------
   --  Element  --
   ---------------

   function Element
     (C : in Cursor)
      return Contact_Entity
   is
      use Common;
   begin
      return Contact_Entity'
        (Ce_Id                   => C.Integer_Value (0, Default => 0),
         Ce_Id_Column_Name       => U ("contactentity_id"),
         Ce_Name                 => U (C.Value (1)),
         Ce_Name_Column_Name     => U (C.Field_Name (1)),
         Is_Human                => C.Boolean_Value (2),
         Is_Human_Column_Name    => U (C.Field_Name (2)),
         Attr_JSON               => C.Json_Object_Value (3),
         Attr_Org_Id             => C.Integer_Value (4, Default => 0),
         Attr_Org_Id_Column_Name => U ("organization_id"));
   end Element;

   ----------------
   --  For_Each  --
   ----------------

   procedure For_Each
     (Org_Id  : in Organization_Id;
      Process : not null access
        procedure (Element : in Contact_Entity))
   is
   begin
      null;
   end For_Each;

   ----------------
   --  For_Each  --
   ----------------

   procedure For_Each
     (Ce_Id   : in Contactentity_Id;
      Process : not null access
        procedure (Element : in Contact_Entity))
   is
      use Database;
      use GNATCOLL.SQL;
      use GNATCOLL.SQL.Exec;

      Get_Contact_Full_Left_Join : constant SQL_Left_Join_Table
        :=  Left_Join (Full    => Contactentity,
                       Partial => Contactentity_Attributes,
                       On      =>
                         Contactentity_Attributes.FK (Contactentity));

      Get_Contact_Full : constant SQL_Query
        := SQL_Select (Fields =>
                         Contactentity.Id &                         --  0
                         Contactentity.Full_Name &                  --  1
                         Contactentity.Is_Human &                   --  2
                         Contactentity_Attributes.Json &            --  3
                         Contactentity_Attributes.Organization_Id,  --  4
                       From   => Get_Contact_Full_Left_Join,
                       Where  => Contactentity.Id = Integer_Param (1));

      Prepared_Get_Contact_Full : constant Prepared_Statement
        := Prepare (Query         => Get_Contact_Full,
                    Auto_Complete => True,
                    On_Server     => True,
                    Name          => "contact");

      Parameters : constant SQL_Parameters := (1 => +Integer (Ce_Id));
   begin
      Bar (Get_Element      => Element'Access,
           Process_Element  => Process,
           Query            => Prepared_Get_Contact_Full,
           Query_Parameters => Parameters);
   end For_Each;

end Model.Contact;
