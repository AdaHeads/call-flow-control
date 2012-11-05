-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                            Organization_List                              --
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
with System_Message.Error;

package body Organization_List is

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

      A_Row     : Row;
      JSON      : JSON_Value;
      Org_Array : JSON_Array;
      Org_JSON  : JSON_Value;
   begin
      JSON := Create_Object;

      while C.Has_Row loop
         A_Row := C.Element;

         Org_JSON := A_Row.Org_JSON;

         Org_JSON.Set_Field (To_String (A_Row.Org_Id_Column_Name),
                             A_Row.Org_Id);

         Org_JSON.Set_Field (To_String (A_Row.Org_Name_Column_Name),
                             A_Row.Org_Name);

         Org_JSON.Set_Field (To_String (A_Row.Identifier_Column_Name),
                             A_Row.Identifier);

         Append (Org_Array, Org_JSON);

         C.Next;
      end loop;

      JSON.Set_Field ("organization_list", Org_Array);

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
                  Identifier_Column_Name => U (C.Field_Name (3)));
   end Element;

   -----------------------------------------
   --  Generic_Constraint_Error_Response  --
   -----------------------------------------

   procedure Generic_Constraint_Error_Response
     (Response_Object :    out Response.Object;
      Message         : in     String)
   is
      use System_Message;
   begin
      Error.Generic_Constraint_Error.Notify (Message, Response_Object);
   end Generic_Constraint_Error_Response;

   ---------------------
   --  Get_List_Type  --
   ---------------------

   function Get_List_Type
     (Response_Object : in Response.Object)
      return List_Type
   is
      pragma Unreferenced (Response_Object);
   begin
      return Full;
   end Get_List_Type;

   ----------------------
   --  Prepared_Query  --
   ----------------------

   function Prepared_Query
     return GNATCOLL.SQL.Exec.Prepared_Statement
   is
      use Database;
      use GNATCOLL.SQL;
      use GNATCOLL.SQL.Exec;

      Org_List : constant SQL_Query
        := SQL_Select (Fields =>
                         Organization.Json &       --  0
                         Organization.Id &         --  1
                         Organization.Name &       --  2
                         Organization.Identifier,  --  3
                       From => Organization);

      Prepared_Get_Organization_List : constant Prepared_Statement
        := Prepare (Query         => Org_List,
                    Auto_Complete => True,
                    On_Server     => True,
                    Name          => "organization_list");
   begin
      return Prepared_Get_Organization_List;
   end Prepared_Query;

   ------------------------
   --  Query_Parameters  --
   ------------------------

   function Query_Parameters
     (Response_Object : in Response.Object)
      return GNATCOLL.SQL.Exec.SQL_Parameters
   is
      pragma Unreferenced (Response_Object);

      use GNATCOLL.SQL.Exec;
   begin
      return No_Parameters;
   end Query_Parameters;

end Organization_List;
