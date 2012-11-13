-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                              Model.Contacts                                --
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
with View.Contact;

package body Model.Contacts is

   use GNATCOLL.SQL;
   use GNATCOLL.SQL.Exec;

   package SQL renames SQL_Statements;

   procedure Fetch_Contact_Object is new Storage.Process_Query
     (Database_Cursor   => Cursor,
      Element           => Contact_Object,
      Cursor_To_Element => Contact_Element);

   procedure Fetch_Contact_Objects is new Storage.Process_Query
     (Database_Cursor   => Cursor,
      Element           => Contact_Object,
      Cursor_To_Element => Contact_Elements);

   ---------------------
   --  Add_Attribute  --
   ---------------------

   procedure Add_Attribute
     (Contact   : in out Contact_Object;
      Attribute : in     Model.Contacts_Attributes.Contact_Attributes_Object)
   is
   begin
      Contact.Attr_Map.Include
        (Map_Key (Attribute.Contact_Id, Attribute.Organization_Id),
         Attribute);
   end Add_Attribute;

   ------------------
   --  Attributes  --
   ------------------

   function Attributes
     (Contact : in Contact_Object)
      return Attributes_Map.Map
   is
   begin
      return Contact.Attr_Map;
   end Attributes;

   -----------------------
   --  Contact_Element  --
   -----------------------

   function Contact_Element
     (C : in out Cursor)
      return Contact_Object'Class
   is
      use Common;

      CO : Contact_Object;
   begin
      CO := Contact_Object'
        (Attr_Map  => Attributes_Map.Empty_Map,
         C_Id      => Contact_Identifier (C.Integer_Value (0, Default => 0)),
         Full_Name => U (C.Value (1)),
         Is_Human  => C.Boolean_Value (2));

      while C.Has_Row loop
         if not C.Is_Null (3) then
            CO.Attr_Map.Include
              (Key     => Map_Key
                 (Contact_Identifier (C.Integer_Value (4, Default => 0)),
                  Organization_Identifier (C.Integer_Value (5, Default => 0))),
              New_Item => Model.Contacts_Attributes.Create
              (JSON => C.Json_Object_Value (3),
               C_Id => Contact_Identifier (C.Integer_Value (4, Default => 0)),
               O_Id => Organization_Identifier
                 (C.Integer_Value (5, Default => 0))));
         end if;

         C.Next;
      end loop;

      return CO;
   end Contact_Element;

   ------------------------
   --  Contact_Elements  --
   ------------------------

   function Contact_Elements
     (C : in out Cursor)
      return Contact_Object'Class
   is
      use Common;

      CO : Contact_Object;
   begin
      CO := Contact_Object'
        (Attr_Map  => Attributes_Map.Empty_Map,
         C_Id      => Contact_Identifier (C.Integer_Value (0, Default => 0)),
         Full_Name => U (C.Value (1)),
         Is_Human  => C.Boolean_Value (2));

      if not C.Is_Null (3) then
         CO.Attr_Map.Include
           (Key      => Map_Key
              (Contact_Identifier (C.Integer_Value (4, Default => 0)),
               Organization_Identifier (C.Integer_Value (5, Default => 0))),
            New_Item => Model.Contacts_Attributes.Create
              (JSON => C.Json_Object_Value (3),
               C_Id => Contact_Identifier (C.Integer_Value (4, Default => 0)),
               O_Id => Organization_Identifier
                 (C.Integer_Value (5, Default => 0))));
      end if;

      return CO;
   end Contact_Elements;

   ------------------
   --  Contact_Id  --
   ------------------

   function Contact_Id
     (Contact : in Contact_Object)
      return Contact_Identifier
   is
   begin
      return Contact.C_Id;
   end Contact_Id;

   --------------
   --  Create  --
   --------------

   function Create
     (C_Id      : in Contact_Identifier;
      Full_Name : in String;
      Is_Human  : in Boolean)
      return Contact_Object
   is
      use Common;
   begin
      return Contact_Object'(Attr_Map  => Attributes_Map.Empty_Map,
                             C_Id      => C_Id,
                             Full_Name => U (Full_Name),
                             Is_Human  => Is_Human);
   end Create;

   -------------
   --  Equal  --
   -------------

   function Equal
     (Left, Right : in Model.Contacts_Attributes.Contact_Attributes_Object)
      return Boolean
   is
      use type Model.Contacts_Attributes.Contact_Attributes_Object;
   begin
      return Left = Right;
   end Equal;

   -----------------
   --  Full_Name  --
   -----------------

   function Full_Name
     (Contact : in Contact_Object)
      return String
   is
   begin
      return To_String (Contact.Full_Name);
   end Full_Name;

   -----------
   --  Get  --
   -----------

   procedure Get
     (C_Id    : in Contact_Identifier;
      Process : not null access
        procedure (Element : in Contact_Object'Class))
   is
      Parameters : constant SQL_Parameters := (1 => +Integer (C_Id));
   begin
      Fetch_Contact_Object
        (Process_Element    => Process,
         Prepared_Statement => SQL.Prepared_Contact_Query_Full,
         Query_Parameters   => Parameters);
   end Get;

   -----------
   --  Get  --
   -----------

   procedure Get
     (C_Id    : in Contact_Identifier;
      O_Id    : in Organization_Identifier;
      Process : not null access
        procedure (Element : in Contact_Object'Class))
   is
      Parameters : constant SQL_Parameters := (1 => +Integer (C_Id),
                                               2 => +Integer (O_Id));
   begin
      Fetch_Contact_Object
        (Process_Element    => Process,
         Prepared_Statement => SQL.Prepared_Contact_Org_Specified_Query,
         Query_Parameters   => Parameters);
   end Get;

   -----------
   --  Get  --
   -----------

   procedure Get
     (O_Id    : in Organization_Identifier;
      Process : not null access
        procedure (Element : in Contact_Object'Class))
   is
      Parameters : constant SQL_Parameters := (1 => +Integer (O_Id));
   begin
      Fetch_Contact_Objects
        (Process_Element    => Process,
         Prepared_Statement => SQL.Prepared_Organization_Contacts_Query,
         Query_Parameters   => Parameters);
   end Get;

   -----------
   --  Get  --
   -----------

   function Get
     (C_Id : in Contact_Identifier)
      return Contact_Object
   is
      procedure Get_Element
        (Contact : in Contact_Object'Class);

      C : Contact_Object := Null_Contact_Object;

      -------------------
      --  Get_Element  --
      -------------------

      procedure Get_Element
        (Contact : in Contact_Object'Class)
      is
      begin
         C := Contact_Object (Contact);
      end Get_Element;
   begin
      Get (C_Id, Get_Element'Access);
      return C;
   end Get;

   -----------
   --  Get  --
   -----------

   function Get
     (C_Id : in Contact_Identifier;
      O_Id : in Organization_Identifier)
      return Contact_Object
   is
      procedure Get_Element
        (Contact : in Contact_Object'Class);

      C : Contact_Object := Null_Contact_Object;

      -------------------
      --  Get_Element  --
      -------------------

      procedure Get_Element
        (Contact : in Contact_Object'Class)
      is
      begin
         C := Contact_Object (Contact);
      end Get_Element;
   begin
      Get (C_Id, O_Id, Get_Element'Access);
      return C;
   end Get;

   ----------------
   --  Is_Human  --
   ----------------

   function Is_Human
     (Contact : in Contact_Object)
      return Boolean
   is
   begin
      return Contact.Is_Human;
   end Is_Human;

   ---------------
   --  To_JSON  --
   ---------------

   function To_JSON
     (Contact : in Contact_Object)
      return Common.JSON_String
   is
   begin
      return View.Contact.To_JSON (Contact);
   end To_JSON;

end Model.Contacts;
