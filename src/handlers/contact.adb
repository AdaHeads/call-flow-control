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
with System_Message.Error;

package body Contact is

   ---------------------
   --  Bad_Ce_Id_Key  --
   ---------------------

   procedure Bad_Ce_Id_Key
     (Response_Object :    out Response.Object;
      Message         : in     String)
   is
      use System_Message;
   begin
      Error.Bad_Ce_Id_Key.Notify (Message, Response_Object);
   end Bad_Ce_Id_Key;

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

      procedure Build_Attribute_JSON;
      --  Build an attribute node for a contact entity.

      A_Row      : Row;
      Attr_Array : JSON_Array;
      Attr_JSON  : JSON_Value;
      JSON       : JSON_Value;

      ----------------------------
      --  Build_Attribute_JSON  --
      ----------------------------

      procedure Build_Attribute_JSON
      is
      begin
         if A_Row.Attr_JSON /= JSON_Null then
            Attr_JSON := A_Row.Attr_JSON;

            Attr_JSON.Set_Field
              (To_String (A_Row.Attr_Org_Id_Column_Name),
               A_Row.Attr_Org_Id);

            Append (Attr_Array, Attr_JSON);
         end if;
      end Build_Attribute_JSON;
   begin
      JSON := Create_Object;

      if C.Has_Row then
         A_Row := C.Element;

         JSON.Set_Field (To_String (A_Row.Ce_Id_Column_Name),
                         A_Row.Ce_Id);

         JSON.Set_Field (To_String (A_Row.Ce_Name_Column_Name),
                         A_Row.Ce_Name);

         JSON.Set_Field (To_String (A_Row.Is_Human_Column_Name),
                         A_Row.Is_Human);

         Build_Attribute_JSON;

         C.Next;

         while C.Has_Row loop
            --  If we have more than one row, then we have more than one set of
            --  attributes.
            A_Row := C.Element;
            Build_Attribute_JSON;
            C.Next;
         end loop;

         if Length (Attr_Array) > 0 then
            JSON.Set_Field ("attributes", Attr_Array);
         end if;
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
      return Row'(Ce_Id                   => C.Integer_Value (0, Default => 0),
                  Ce_Id_Column_Name       => U ("contactentity_id"),
                  Ce_Name                 => U (C.Value (1)),
                  Ce_Name_Column_Name     => U (C.Field_Name (1)),
                  Is_Human                => C.Boolean_Value (2),
                  Is_Human_Column_Name    => U (C.Field_Name (2)),
                  Attr_JSON               => C.Json_Object_Value (3),
                  Attr_Org_Id             => C.Integer_Value (4, Default => 0),
                  Attr_Org_Id_Column_Name => U ("organization_id"));
   end Element;

   ---------------------
   --  Get_Ce_Id_Key  --
   ---------------------

   function Get_Ce_Id_Key
     (Response_Object : in Response.Object)
      return Natural
   is
      use AWS.Status;
   begin
      return Natural'Value
        (Parameters (Response_Object.Get_Request).Get ("ce_id"));
   end Get_Ce_Id_Key;

   ----------------------
   --  Prepared_Query  --
   ----------------------

   function Prepared_Query
     return GNATCOLL.SQL.Exec.Prepared_Statement
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
   begin
      return Prepared_Get_Contact_Full;
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
      return (1 => +Get_Ce_Id_Key (Response_Object));
   end Query_Parameters;

end Contact;
