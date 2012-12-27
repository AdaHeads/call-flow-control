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

with Common;
with Model.Attribute;
with SQL_Statements.Contact;
with Storage;
with View.Contact;

package body Model.Contacts is

   use GNATCOLL.SQL;
   use GNATCOLL.SQL.Exec;

   package SQL renames SQL_Statements.Contact;

   function Contact_List
     (Cursor : in out Database_Cursor'Class)
      return List;
   --  Transforms the low level index based Cursor into a List object.

   procedure Process_Select_Query is new Storage.Process_Select_Query
     (Database_Cursor   => Database_Cursor,
      Element           => List,
      Cursor_To_Element => Contact_List);

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
        (Key      => Organization_Contact_Identifier'
           (CID => Contact.ID,
            OID => ID),
         New_Item => Contact);
   end Add_Contact;

   -----------------------
   --  Contact_List  --
   -----------------------

   function Contact_List
     (Cursor : in out Database_Cursor'Class)
      return List
   is
      use Common;
      use Model.Contact;

      A_Id    : Attribute_Identifier;
      A_List  : List;
      Contact : Object;
   begin
      while Cursor.Has_Row loop
         Contact := Create
           (ID        => Contact_Identifier
              (Cursor.Integer_Value (0)),
            Full_Name => Cursor.Value (1),
            Is_Human  => Cursor.Boolean_Value (2));

         if not Cursor.Is_Null (3) then
            A_Id := (Contact_Identifier (Cursor.Integer_Value (4)),
                     Organization_Identifier
                       (Cursor.Integer_Value (5)));

            Contact.Add_Attribute
              (Model.Attribute.Create
                 (ID   => A_Id,
                  JSON => String_To_JSON_Object (Cursor.Json_Text_Value (3))));
         end if;

         A_List.Add_Contact
           (Contact => Contact,
            ID      => Organization_Identifier (Cursor.Integer_Value (6)));

         Cursor.Next;
      end loop;

      return A_List;
   end Contact_List;

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
     (Instance : in List;
      Process  : not null access procedure (Element : in Model.Contact.Object))
   is
   begin
      for Elem of Instance.Contacts loop
         Process (Elem);
      end loop;
   end For_Each;

   -----------
   --  Get  --
   -----------

   function Get
     (ID : in Organization_Identifier)
      return List
   is
      procedure Get_Element
        (Instance : in List);

      Contacts : List;

      -------------------
      --  Get_Element  --
      -------------------

      procedure Get_Element
        (Instance : in List)
      is
      begin
         Contacts := Instance;
      end Get_Element;

      Parameters : constant SQL_Parameters := (1 => +Integer (ID));
   begin
      Process_Select_Query
        (Process_Element    => Get_Element'Access,
         Prepared_Statement => SQL.Contacts_Prepared,
         Query_Parameters   => Parameters);

      return Contacts;
   end Get;

   ----------------
   --  Key_Hash  --
   ----------------

   function Key_Hash
     (Key : in Organization_Contact_Identifier)
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
