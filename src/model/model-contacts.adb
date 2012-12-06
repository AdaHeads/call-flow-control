-------------------------------------------------------------------------------
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
with Model.Attribute;
with SQL_Statements.Contact;
with Storage;
with View.Contact;

package body Model.Contacts is

   use GNATCOLL.SQL;
   use GNATCOLL.SQL.Exec;

   package SQL renames SQL_Statements.Contact;

   function Contact_Element
     (Cursor : in out Database_Cursor'Class)
      return Model.Contact.Object;
   --  Transforms the low level index based Cursor into ONE contact Object
   --  record.

   procedure Fetch_Contact_Objects is new Storage.Process_Select_Query
     (Database_Cursor   => Database_Cursor,
      Element           => Model.Contact.Object,
      Cursor_To_Element => Contact_Element);

   -------------------
   --  Add_Contact  --
   -------------------

   procedure Add_Contact
     (Instance : in out List;
      Contact  : in     Model.Contact.Object;
      ID       : in     Organization_Identifier)
   is
   begin
      Instance.Contacts.Include
        (Key      => Organization_Contact_ID'
           (CID => Contact.ID,
            OID => ID),
         New_Item => Contact);
   end Add_Contact;

   -----------------------
   --  Contact_Element  --
   -----------------------

   function Contact_Element
     (Cursor : in out Database_Cursor'Class)
      return Model.Contact.Object
   is
      use Model.Contact;

      A_Id    : Attribute_ID;
      Contact : Object;
   begin
      Contact := Create
        (ID        => Contact_Identifier
           (Cursor.Integer_Value (0, Default => 0)),
         Full_Name => Cursor.Value (1),
         Is_Human  => Cursor.Boolean_Value (2));

      if not Cursor.Is_Null (3) then
         A_Id := (Contact_Identifier (Cursor.Integer_Value (4, Default => 0)),
                  Organization_Identifier
                    (Cursor.Integer_Value (5, Default => 0)));

         Contact.Add_Attribute
           (Model.Attribute.Create (ID   => A_Id,
                                    JSON => Cursor.Json_Object_Value (3)));
      end if;

      return Contact;
   end Contact_Element;

   ----------------------
   --  Equal_Elements  --
   ----------------------

   function Equal_Elements
     (Left, Right : in Model.Contact.Object)
      return Boolean
   is
      use type Model.Contact.Object;
   begin
      return Left = Right;
   end Equal_Elements;

   -----------------------
   --  Equivalent_Keys  --
   -----------------------

   function Equivalent_Keys
     (Left, Right : in Organization_Contact_ID)
      return Boolean
   is
   begin
      return Left = Right;
   end Equivalent_Keys;

   ----------------
   --  For_Each  --
   ----------------

   procedure For_Each
     (Instance : in List;
      Process  : not null access procedure (Element : in Model.Contact.Object))
   is
   begin
      for Elem of Instance.Contacts loop
         Process (Elem);
      end loop;
   end For_Each;

   ----------------
   --  For_Each  --
   ----------------

   procedure For_Each
     (ID      : in Organization_Identifier;
      Process : not null access procedure (Element : in Model.Contact.Object))
   is
      Parameters : constant SQL_Parameters := (1 => +Integer (ID));
   begin
      Fetch_Contact_Objects
        (Process_Element    => Process,
         Prepared_Statement => SQL.Contacts_Prepared,
         Query_Parameters   => Parameters);
   end For_Each;

   -----------
   --  Get  --
   -----------

   function Get
     (ID : in Organization_Identifier)
      return List
   is
      procedure Get_Element
        (Contact : in Model.Contact.Object);

      O : List;

      -------------------
      --  Get_Element  --
      -------------------

      procedure Get_Element
        (Contact : in Model.Contact.Object)
      is
      begin
         O.Contacts.Include
           (Key      => Organization_Contact_ID'
              (CID => Contact.ID, OID => ID),
            New_Item => Contact);
      end Get_Element;
   begin
      For_Each (ID, Get_Element'Access);
      return O;
   end Get;

   ----------------
   --  Key_Hash  --
   ----------------

   function Key_Hash
     (Key : in Organization_Contact_ID)
      return Ada.Containers.Hash_Type
   is
   begin
      return Ada.Strings.Hash
        (Contact_Identifier'Image (Key.CID) &
           Organization_Identifier'Image (Key.OID));
   end Key_Hash;

   ---------------------
   --  To_JSON_Array  --
   ---------------------

   function To_JSON_Array
     (Instance : in List)
      return GNATCOLL.JSON.JSON_Array
   is
   begin
      return View.Contact.To_JSON_Array (Instance);
   end To_JSON_Array;

end Model.Contacts;
