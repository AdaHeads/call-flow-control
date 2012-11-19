-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                        Model.Contacts_Attributes                          --
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

with SQL_Statements;
with Storage;
with View.Contact_Attributes;

package body Model.Contacts_Attributes is

   use GNATCOLL.SQL;
   use GNATCOLL.SQL.Exec;

   package SQL renames SQL_Statements;

   procedure Fetch_Contact_Attributes_Object is new Storage.Process_Query
     (Database_Cursor   => Cursor,
      Element           => Contact_Attributes_Object,
      Cursor_To_Element => Contact_Attributes_Element);

   ----------------------------------
   --  Contact_Attributes_Element  --
   ----------------------------------

   function Contact_Attributes_Element
     (C : in out Cursor)
      return Contact_Attributes_Object'Class
   is
   begin
      return Contact_Attributes_Object'
        (Id   => Attributes_Identifier'
           (C_Id => Contact_Identifier (C.Integer_Value (0, Default => 0)),
            O_Id => Organization_Identifier
              (C.Integer_Value (1, Default => 0))),
         JSON => C.Json_Object_Value (2));
   end Contact_Attributes_Element;

   ------------------
   --  Contact_Id  --
   ------------------

   function Contact_Id
     (Contact_Attributes : in Contact_Attributes_Object)
      return Contact_Identifier
   is
   begin
      return Contact_Attributes.Id.C_Id;
   end Contact_Id;

   --------------
   --  Create  --
   --------------

   function Create
     (Id   : in Attributes_Identifier;
      JSON : in GNATCOLL.JSON.JSON_Value)
      return Contact_Attributes_Object
   is
   begin
      return Contact_Attributes_Object'
        (Attributes_Identifier'(C_Id => Id.C_Id, O_Id => Id.O_Id),
         JSON => JSON);
   end Create;

   ----------------
   --  For_Each  --
   ----------------

   procedure For_Each
     (C_Id    : in Contact_Identifier;
      Process : not null access
        procedure (Element : in Contact_Attributes_Object'Class))
   is
      Parameters : constant SQL_Parameters := (1 => +Integer (C_Id));
   begin
      Fetch_Contact_Attributes_Object
        (Process_Element    => Process,
         Prepared_Statement => SQL.Contact_Attributes_Prepared,
         Query_Parameters   => Parameters);
   end For_Each;

   ----------------
   --  For_Each  --
   ----------------

   procedure For_Each
     (Id      : in Attributes_Identifier;
      Process : not null access
        procedure (Element : in Contact_Attributes_Object'Class))
   is
      Parameters : constant SQL_Parameters := (1 => +Integer (Id.C_Id),
                                               2 => +Integer (Id.O_Id));
   begin
      Fetch_Contact_Attributes_Object
        (Process_Element    => Process,
         Prepared_Statement => SQL.Contact_Organization_Attributes_Prepared,
         Query_Parameters   => Parameters);
   end For_Each;

   -----------
   --  Get  --
   -----------

   function Get
     (Id : in Attributes_Identifier)
      return Contact_Attributes_Object
   is
      procedure Get_Element
        (Contact_Attributes : in Contact_Attributes_Object'Class);

      CA : Contact_Attributes_Object := Null_Contact_Attributes;

      -------------------
      --  Get_Element  --
      -------------------

      procedure Get_Element
        (Contact_Attributes : in Contact_Attributes_Object'Class)
      is
      begin
         CA := Contact_Attributes_Object (Contact_Attributes);
      end Get_Element;
   begin
      For_Each (Id, Get_Element'Access);
      return CA;
   end Get;

   ------------
   --  JSON  --
   ------------

   function JSON
     (Contact_Attributes : Contact_Attributes_Object)
      return GNATCOLL.JSON.JSON_Value
   is
   begin
      return Contact_Attributes.JSON;
   end JSON;

   -----------------------
   --  Organization_Id  --
   -----------------------

   function Organization_Id
     (Contact_Attributes : Contact_Attributes_Object)
      return Organization_Identifier
   is
   begin
      return Contact_Attributes.Id.O_Id;
   end Organization_Id;

   ---------------
   --  To_JSON  --
   ---------------

   function To_JSON
     (Contact_Attributes : in Contact_Attributes_Object)
      return Common.JSON_String
   is
   begin
      return View.Contact_Attributes.To_JSON (Contact_Attributes);
   end To_JSON;

end Model.Contacts_Attributes;
