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

with Ada.Strings.Hash;
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
     (Self      : in out Contact_Object;
      Attribute : in     Model.Contact_Attributes.Contact_Attributes_Object)
   is
   begin
      Self.Attr_List.Add_Attributes (Attribute);
   end Add_Attribute;

   -------------------
   --  Add_Contact  --
   -------------------

   procedure Add_Contact
     (Self    : in out Contact_List_Object;
      Contact : in     Contact_Object'Class;
      O_ID    : in     Organization_Identifier)
   is
   begin
      Self.Contacts.Include
        (Key      => Organization_Contact_Identifier'(C_ID => Contact.C_ID,
                                                      O_ID => O_ID),
         New_Item => Contact_Object (Contact));
   end Add_Contact;

   ------------------
   --  Attributes  --
   ------------------

   function Attributes
     (Self : in Contact_Object)
      return Model.Contact_Attributes.Contact_Attributes_List_Object
   is
   begin
      return Self.Attr_List;
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
        (Attr_List => Model.Contact_Attributes.Null_Contact_Attributes_List,
         C_ID      => Contact_Identifier (C.Integer_Value (0, Default => 0)),
         Full_Name => U (C.Value (1)),
         Is_Human  => C.Boolean_Value (2));

      while C.Has_Row loop
         if not C.Is_Null (3) then
            CO.Attr_List.Add_Attributes
              (Model.Contact_Attributes.Create
                 (ID   => Attributes_Identifier'
                    (C_ID => Contact_Identifier
                       (C.Integer_Value (4, Default => 0)),
                     O_ID => Organization_Identifier
                       (C.Integer_Value (5, Default => 0))),
                  JSON => C.Json_Object_Value (3)));
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

      A_Id : Attributes_Identifier;
      CO   : Contact_Object;
   begin
      CO := Contact_Object'
        (Attr_List => Model.Contact_Attributes.Null_Contact_Attributes_List,
         C_ID      => Contact_Identifier (C.Integer_Value (0, Default => 0)),
         Full_Name => U (C.Value (1)),
         Is_Human  => C.Boolean_Value (2));

      if not C.Is_Null (3) then
         A_Id := (Contact_Identifier (C.Integer_Value (4, Default => 0)),
                  Organization_Identifier (C.Integer_Value (5, Default => 0)));

         CO.Attr_List.Add_Attributes
           (Model.Contact_Attributes.Create
              (ID   => A_Id,
               JSON => C.Json_Object_Value (3)));
      end if;

      return CO;
   end Contact_Elements;

   ------------------
   --  Contact_ID  --
   ------------------

   function Contact_ID
     (Self : in Contact_Object)
      return Contact_Identifier
   is
   begin
      return Self.C_ID;
   end Contact_ID;

   --------------
   --  Create  --
   --------------

   function Create
     (C_ID      : in Contact_Identifier;
      Full_Name : in String;
      Is_Human  : in Boolean)
      return Contact_Object
   is
      use Common;
   begin
      return Contact_Object'
        (Attr_List => Model.Contact_Attributes.Null_Contact_Attributes_List,
         C_ID      => C_ID,
         Full_Name => U (Full_Name),
         Is_Human  => Is_Human);
   end Create;

   ----------------------
   --  Equal_Elements  --
   ----------------------

   function Equal_Elements
     (Left, Right : in Contact_Object)
      return Boolean
   is
      use type Model.Contacts.Contact_Object;
   begin
      return Left = Right;
   end Equal_Elements;

   -----------------------
   --  Equivalent_Keys  --
   -----------------------

   function Equivalent_Keys
     (Left, Right : in Organization_Contact_Identifier)
      return Boolean
   is
   begin
      return Left = Right;
   end Equivalent_Keys;

   ----------------
   --  For_Each  --
   ----------------

   procedure For_Each
     (C_ID    : in Contact_Identifier;
      Process : not null access
        procedure (Element : in Contact_Object'Class))
   is
      Parameters : constant SQL_Parameters := (1 => +Integer (C_ID));
   begin
      Fetch_Contact_Object
        (Process_Element    => Process,
         Prepared_Statement => SQL.Contact_Full_Prepared,
         Query_Parameters   => Parameters);
   end For_Each;

   ----------------
   --  For_Each  --
   ----------------

   procedure For_Each
     (O_ID    : in Organization_Identifier;
      Process : not null access
        procedure (Element : in Contact_Object'Class))
   is
      Parameters : constant SQL_Parameters := (1 => +Integer (O_ID));
   begin
      Fetch_Contact_Objects
        (Process_Element    => Process,
         Prepared_Statement => SQL.Contacts_Prepared,
         Query_Parameters   => Parameters);
   end For_Each;

   ----------------
   --  For_Each  --
   ----------------

   procedure For_Each
     (C_ID    : in Contact_Identifier;
      O_ID    : in Organization_Identifier;
      Process : not null access
        procedure (Element : in Contact_Object'Class))
   is
      Parameters : constant SQL_Parameters := (1 => +Integer (C_ID),
                                               2 => +Integer (O_ID));
   begin
      Fetch_Contact_Object
        (Process_Element    => Process,
         Prepared_Statement => SQL.Contact_Org_Specified_Prepared,
         Query_Parameters   => Parameters);
   end For_Each;

   ----------------
   --  For_Each  --
   ----------------

   procedure For_Each
     (Self    : in Contact_List_Object;
      Process : not null access
        procedure (Element : in Contact_Object))
   is
   begin
      for Elem of Self.Contacts loop
         Process (Elem);
      end loop;
   end For_Each;

   -----------------
   --  Full_Name  --
   -----------------

   function Full_Name
     (Self : in Contact_Object)
      return String
   is
   begin
      return To_String (Self.Full_Name);
   end Full_Name;

   -----------
   --  Get  --
   -----------

   procedure Get
     (Self : in out Contact_Object;
      C_ID : in     Contact_Identifier)
   is
      procedure Get_Element
        (Contact : in Contact_Object'Class);

      -------------------
      --  Get_Element  --
      -------------------

      procedure Get_Element
        (Contact : in Contact_Object'Class)
      is
      begin
         Self := Contact_Object (Contact);
      end Get_Element;
   begin
      For_Each (C_ID, Get_Element'Access);
   end Get;

   -----------
   --  Get  --
   -----------

   procedure Get
     (Self : in out Contact_List_Object;
      O_ID : in     Organization_Identifier)
   is
      procedure Get_Element
        (Contact : in Contact_Object'Class);

      -------------------
      --  Get_Element  --
      -------------------

      procedure Get_Element
        (Contact : in Contact_Object'Class)
      is
      begin
         Self.Contacts.Include
           (Key      => Organization_Contact_Identifier'
              (C_ID => Contact.C_ID, O_ID => O_ID),
            New_Item => Contact_Object (Contact));
      end Get_Element;
   begin
      For_Each (O_ID, Get_Element'Access);
   end Get;

   -----------
   --  Get  --
   -----------

   function Get
     (C_ID : in Contact_Identifier;
      O_ID : in Organization_Identifier)
      return Contact_Object
   is
      procedure Get_Element
        (Contact : in Contact_Object'Class);

      C : Contact_Object := Null_Contact;

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
      For_Each (C_ID, O_ID, Get_Element'Access);
      return C;
   end Get;

   ----------------
   --  Is_Human  --
   ----------------

   function Is_Human
     (Self : in Contact_Object)
      return Boolean
   is
   begin
      return Self.Is_Human;
   end Is_Human;

   ----------------
   --  Key_Hash  --
   ----------------

   function Key_Hash
     (Key : in Organization_Contact_Identifier)
      return Ada.Containers.Hash_Type
   is
   begin
      return Ada.Strings.Hash
        (Contact_Identifier'Image (Key.C_ID) &
           Organization_Identifier'Image (Key.O_ID));
   end Key_Hash;

   ---------------
   --  To_JSON  --
   ---------------

   function To_JSON
     (Self : in Contact_Object)
      return Common.JSON_String
   is
   begin
      return View.Contact.To_JSON (Self);
   end To_JSON;

end Model.Contacts;
