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
with GNATCOLL.JSON;
with SQL_Statements.Organization;
with Storage;
with View.Organization;

package body Model.Organizations is

   package SQL renames SQL_Statements.Organization;

   function Organization_Midi
     (C : in out Database_Cursor'Class)
      return Organization_List_Object;

   function Organization_Mini
     (C : in out Database_Cursor'Class)
      return Organization_List_Object;

   procedure Process_Select_Query_Midi is new Storage.Process_Select_Query
     (Database_Cursor   => Database_Cursor,
      Element           => Organization_List_Object,
      Cursor_To_Element => Organization_Midi);

   procedure Process_Select_Query_Mini is new Storage.Process_Select_Query
     (Database_Cursor   => Database_Cursor,
      Element           => Organization_List_Object,
      Cursor_To_Element => Organization_Mini);

   ----------------------
   --  Equal_Elements  --
   ----------------------

   function Equal_Elements
     (Left, Right : in Model.Organization.Organization_Object)
      return Boolean
   is
      use type Model.Organization.Organization_Object;
   begin
      return Left = Right;
   end Equal_Elements;

   -----------------------
   --  Equivalent_Keys  --
   -----------------------

   function Equivalent_Keys
     (Left, Right : in Organization_Identifier)
      return Boolean
   is
   begin
      return Left = Right;
   end Equivalent_Keys;

   ----------------
   --  For_Each  --
   ----------------

   procedure For_Each
     (Instance : in Organization_List_Object;
      Process  : not null access procedure
        (Element : in Model.Organization.Organization_Object))
   is
   begin
      for Elem of Instance.Organizations loop
         Process (Elem);
      end loop;
   end For_Each;

   -----------
   --  Get  --
   -----------

   function Get
     (Mode : in Data_Mode := Mini)
      return Organization_List_Object
   is
      use GNATCOLL.SQL.Exec;
      use Model.Organization;

      procedure Get_Element
        (Instance : in Organization_List_Object);

      O : Organization_List_Object;

      -------------------
      --  Get_Element  --
      -------------------

      procedure Get_Element
        (Instance : in Organization_List_Object)
      is
      begin
         O := Instance;
      end Get_Element;
   begin
      case Mode is
         when Mini =>
            Process_Select_Query_Mini
              (Process_Element    => Get_Element'Access,
               Prepared_Statement => SQL.Organizations_Mini_Prepared,
               Query_Parameters   => No_Parameters);
         when Midi =>
            Process_Select_Query_Midi
              (Process_Element    => Get_Element'Access,
               Prepared_Statement => SQL.Organizations_Midi_Prepared,
               Query_Parameters   => No_Parameters);
      end case;

      return O;
   end Get;

   ----------------
   --  Key_Hash  --
   ----------------

   function Key_Hash
     (Key : in Organization_Identifier)
      return Ada.Containers.Hash_Type
   is
   begin
      return Ada.Strings.Hash (Organization_Identifier'Image (Key));
   end Key_Hash;

   -------------------------
   --  Organization_Midi  --
   -------------------------

   function Organization_Midi
     (C : in out Database_Cursor'Class)
      return Organization_List_Object
   is
      use Model.Organization;

      A_List : Organization_List_Object;
      O : Model.Organization.Organization_Object;
   begin
      while C.Has_Row loop
         O := Model.Organization.Create
           (ID         => Organization_Identifier (C.Integer_Value (3)),
            Full_Name  => C.Value (0),
            Identifier => C.Value (1),
            JSON       => C.Json_Object_Value (2),
            Mode       => Maxi);

         A_List.Organizations.Include
           (Key      => O.ID,
            New_Item => O);

         C.Next;
      end loop;

      return A_List;
   end Organization_Midi;

   ------------------------
   --  Organization_Mini --
   ------------------------

   function Organization_Mini
     (C : in out Database_Cursor'Class)
      return Organization_List_Object
   is
      use Model.Organization;

      A_List : Organization_List_Object;
      O : Model.Organization.Organization_Object;
   begin
      while C.Has_Row loop
         O := Model.Organization.Create
           (ID         => Organization_Identifier (C.Integer_Value (2)),
            Full_Name  => C.Value (0),
            Identifier => C.Value (1),
            JSON       => GNATCOLL.JSON.JSON_Null,
            Mode       => Mini);

         A_List.Organizations.Include
           (Key      => O.ID,
            New_Item => O);

         C.Next;
      end loop;

      return A_List;
   end Organization_Mini;

   ----------------------
   --  To_JSON_String  --
   ----------------------

   function To_JSON_String
     (Self : in out Organization_List_Object)
      return Common.JSON_String
   is
   begin
      return View.Organization.To_JSON_String (Self);
   end To_JSON_String;

end Model.Organizations;
