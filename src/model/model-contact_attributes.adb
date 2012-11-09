-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                         Model.Contact_Attributes                          --
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
with Storage;

package body Model.Contact_Attributes is

   use GNATCOLL.SQL;
   use GNATCOLL.SQL.Exec;

   package DB renames Database;

   procedure Fetch_Contact_Attributes_Object is new Storage.Process_Query
     (Database_Cursor   => Cursor,
      Element           => Contact_Attributes_Object,
      Cursor_To_Element => Contact_Attributes_Element);

   --------------------------------------------------------
   --  Prepared statement for fetching a contact entity  --
   --------------------------------------------------------

   Contact_Attributes_Query : constant SQL_Query
     := SQL_Select (Fields =>
                      DB.Contact_Attributes.Contact_Id &       --  0
                      DB.Contact_Attributes.Organization_Id &  --  1
                      DB.Contact_Attributes.Json,              --  2
                    From   => DB.Contact_Attributes,
                    Where  =>
                      DB.Contact_Attributes.Contact_Id = Integer_Param (1));

   Prepared_Contact_Attributes_Query : constant Prepared_Statement
     := Prepare (Query         => Contact_Attributes_Query,
                 Auto_Complete => True,
                 On_Server     => True,
                 Name          => "contact_attributes");

   ----------------------------------
   --  Contact_Attributes_Element  --
   ----------------------------------

   function Contact_Attributes_Element
     (C : in out Cursor)
      return Contact_Attributes_Object'Class
   is
   begin
      return Contact_Attributes_Object'
        (C_Id => Contact_Id (C.Integer_Value (0, Default => 0)),
         O_Id => Organization_Id (C.Integer_Value (1, Default => 0)),
         JSON => C.Json_Object_Value (2));
   end Contact_Attributes_Element;

   --------------
   --  Create  --
   --------------

   function Create
     (C_Id : in Contact_Id;
      O_Id : in Organization_Id;
      JSON : in GNATCOLL.JSON.JSON_Value)
      return Contact_Attributes_Object
   is
   begin
      return Contact_Attributes_Object'(C_Id => C_Id,
                                        O_Id => O_Id,
                                        JSON => JSON);
   end Create;

   -------------
   --  Equal  --
   -------------

   function Equal
     (Left, Right : in Contact_Attributes_Object)
      return Boolean
   is
   begin
      return Left = Right;
   end Equal;

   -----------
   --  Get  --
   -----------

   procedure Get
     (Id      : in Contact_Id;
      Process : not null access
        procedure (Element : in Contact_Attributes_Object'Class))
   is
      Parameters : constant SQL_Parameters := (1 => +Integer (Id));
   begin
      Fetch_Contact_Attributes_Object
        (Process_Element    => Process,
         Prepared_Statement => Prepared_Contact_Attributes_Query,
         Query_Parameters   => Parameters);
   end Get;

   ----------------------
   --  Get_Contact_Id  --
   ----------------------

   function Get_Contact_Id
     (Contact_Attributes : in Contact_Attributes_Object)
      return Contact_Id
   is
   begin
      return Contact_Attributes.C_Id;
   end Get_Contact_Id;

   ----------------
   --  Get_JSON  --
   ----------------

   function Get_JSON
     (Contact_Attributes : Contact_Attributes_Object)
      return GNATCOLL.JSON.JSON_Value
   is
   begin
      return Contact_Attributes.JSON;
   end Get_JSON;

   ---------------------------
   --  Get_Organization_Id  --
   ---------------------------

   function Get_Organization_Id
     (Contact_Attributes : Contact_Attributes_Object)
      return Organization_Id
   is
   begin
      return Contact_Attributes.O_Id;
   end Get_Organization_Id;

   ----------------
   --  For_Each  --
   ----------------

--     procedure For_Each
--       (Org_Id  : in Organization_Id;
--        Process : not null access
--          procedure (Element : in Contact_Object))
--     is
--     begin
--        null;
--     end For_Each;

   -----------
   --  Get  --
   -----------

--     function Get
--       (Id : in Contact_Id)
--        return Contact_Object
--     is
--        procedure Get_Element
--          (Contact : in Contact_Object);
--
--        C : Contact_Object := Null_Contact_Entity;
--
--        -------------------
--        --  Get_Element  --
--        -------------------
--
--        procedure Get_Element
--          (Contact : in Contact_Object)
--        is
--        begin
--           C := Contact;
--        end Get_Element;
--     begin
--        Get (Id, Get_Element'Access);
--        return C;
--     end Get;

end Model.Contact_Attributes;
