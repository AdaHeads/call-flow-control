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
with SQL_Prepared_Statements.Attribute;
with Storage;
with View.Attribute;

package body Model.Attributes is

   use GNATCOLL.SQL;
   use GNATCOLL.SQL.Exec;

   package SQL renames SQL_Prepared_Statements.Attribute;

   function Attribute_List
     (C : in out Database_Cursor'Class)
      return List;
   --  Transforms the low level index based Cursor into a List object.

   procedure Process_Select_Query is new Storage.Process_Select_Query
     (Database_Cursor   => Database_Cursor,
      Element           => List,
      Cursor_To_Element => Attribute_List);

   ---------------------
   --  Add_Attribute  --
   ---------------------

   procedure Add_Attribute
     (Instance  : in out List;
      Attribute : in     Model.Attribute.Object)
   is
   begin
      Instance.Attributes.Include (Key      => Attribute.ID,
                                   New_Item => Attribute);
   end Add_Attribute;

   ----------------------
   --  Attribute_List  --
   ----------------------

   function Attribute_List
     (C : in out Database_Cursor'Class)
      return List
   is
      use Common;

      A_List : List;
   begin
      while C.Has_Row loop
         A_List.Add_Attribute
           (Model.Attribute.Create
              (ID   => (Contact_Identifier (C.Integer_Value (0, Default => 0)),
                        Organization_Identifier
                          (C.Integer_Value (1, Default => 0))),
               JSON => String_To_JSON_Object (C.Json_Text_Value (2))));

         C.Next;
      end loop;

      return A_List;
   end Attribute_List;

   ----------------------
   --  Equal_Elements  --
   ----------------------

   function Equal_Elements
     (Left, Right : in Model.Attribute.Object)
      return Boolean
   is
      use type Model.Attribute.Object;
   begin
      return Left = Right;
   end Equal_Elements;

   -----------------------
   --  Equivalent_Keys  --
   -----------------------

   function Equivalent_Keys
     (Left, Right : in Attribute_Identifier)
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
      Process  : not null access
        procedure (Element : in Model.Attribute.Object))
   is
   begin
      for Elem of Instance.Attributes loop
         Process (Elem);
      end loop;
   end For_Each;

   -----------
   --  Get  --
   -----------

   function Get
     (ID : in Contact_Identifier)
      return List
   is
      procedure Get_Element
        (Instance : in List);

      Attributes : List;

      -------------------
      --  Get_Element  --
      -------------------

      procedure Get_Element
        (Instance : in List)
      is
      begin
         Attributes := Instance;
      end Get_Element;

      Parameters : constant SQL_Parameters := (1 => +Integer (ID));
   begin
      Process_Select_Query
        (Process_Element    => Get_Element'Access,
         Prepared_Statement => SQL.Contact_Attributes_Prepared,
         Query_Parameters   => Parameters);

      return Attributes;
   end Get;

   ----------------
   --  Key_Hash  --
   ----------------

   function Key_Hash
     (Key : in Attribute_Identifier)
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
      return View.Attribute.To_JSON_Array (Instance);
   end To_JSON_Array;

end Model.Attributes;
