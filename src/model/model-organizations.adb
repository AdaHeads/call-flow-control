-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                           Model.Organizations                             --
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

--  with Model.Contacts_Attributes;
with SQL_Statements;
with Storage;
with View.Organization;

package body Model.Organizations is

   package SQL renames SQL_Statements;

   procedure Fetch_Organization_Object is new Storage.Process_Query
     (Database_Cursor   => Cursor,
      Element           => Organization_Object,
      Cursor_To_Element => Organization_Element);

   procedure For_Each
     (Process : not null access
        procedure (Element : in Organization_Object'Class));
   --  For every organization in the database, an Organization_Object is handed
   --  to Process. These organization objects do NOT contain any contacts.

   --------------------
   --  Contact_List  --
   --------------------

   function Contact_List
     (Self : in Organization_Object)
      return Model.Contacts.Contact_List_Object
   is
   begin
      return Self.Cntacts;
   end Contact_List;

   ----------------------
   --  Equal_Elements  --
   ----------------------

   function Equal_Elements
     (Left, Right : in Model.Contacts.Contact_Object)
      return Boolean
   is
      use type Model.Contacts.Contact_Object;
   begin
      return Left = Right;
   end Equal_Elements;

   ----------------
   --  For_Each  --
   ----------------

   procedure For_Each
     (Process : not null access
        procedure (Element : in Organization_Object'Class))
   is
      use GNATCOLL.SQL.Exec;
   begin
      Fetch_Organization_Object
        (Process_Element    => Process,
         Prepared_Statement => SQL.Organizations_Prepared,
         Query_Parameters   => No_Parameters);
   end For_Each;

   ----------------
   --  For_Each  --
   ----------------

   procedure For_Each
     (Self    : in Organization_List_Object;
      Process : not null access
        procedure (Element : in Organization_Object))
   is
   begin
      for Elem of Self.Org_List loop
         Process (Elem);
      end loop;
   end For_Each;

   ----------------
   --  For_Each  --
   ----------------

   procedure For_Each
     (O_Id    : in Organization_Identifier;
      Process : not null access
        procedure (Element : in Organization_Object'Class))
   is
      use GNATCOLL.SQL.Exec;

      Parameters : constant SQL_Parameters := (1 => +Integer (O_Id));
   begin
      Fetch_Organization_Object
        (Process_Element    => Process,
         Prepared_Statement => SQL.Organization_Prepared,
         Query_Parameters   => Parameters);
   end For_Each;

   -----------------
   --  Full_Name  --
   -----------------

   function Full_Name
     (Self : in Organization_Object)
      return String
   is
   begin
      return To_String (Self.Full_Name);
   end Full_Name;

   -----------
   --  Get  --
   -----------

   procedure Get
     (Self : in out Organization_List_Object)
   is
      procedure Add_To_List
        (O : in Organization_Object'Class);

      -------------------
      --  Add_To_List  --
      -------------------

      procedure Add_To_List
        (O : in Organization_Object'Class)
      is
      begin
         Self.Org_List.Append (Organization_Object (O));
      end Add_To_List;
   begin
      For_Each (Add_To_List'Access);
   end Get;

   -----------
   --  Get  --
   -----------

   procedure Get
     (Self : in out Organization_Object;
      O_Id : in     Organization_Identifier)
   is
      procedure Get_Element
        (Organization : in Organization_Object'Class);

      -------------------
      --  Get_Element  --
      -------------------

      procedure Get_Element
        (Organization : in Organization_Object'Class)
      is
      begin
         Self := Organization_Object (Organization);
      end Get_Element;
   begin
      For_Each (O_Id, Get_Element'Access);
   end Get;

   ----------------
   --  Get_Full  --
   ----------------

   procedure Get_Full
     (Self : in out Organization_Object;
      O_Id : in     Organization_Identifier)
   is
      procedure Get_Element
        (Organization : in Organization_Object'Class);

      CL : Model.Contacts.Contact_List_Object;

      -------------------
      --  Get_Element  --
      -------------------

      procedure Get_Element
        (Organization : in Organization_Object'Class)
      is
      begin
         Self := Organization_Object (Organization);
      end Get_Element;
   begin
      For_Each (O_Id, Get_Element'Access);
      CL.Get (O_Id);
      Self.Cntacts := CL;
   end Get_Full;

   ------------------
   --  Identifier  --
   ------------------

   function Identifier
     (Self : in Organization_Object)
      return String
   is
   begin
      return To_String (Self.Identifier);
   end Identifier;

   ------------
   --  JSON  --
   ------------

   function JSON
     (Self : in Organization_Object)
      return GNATCOLL.JSON.JSON_Value
   is
   begin
      return Self.JSON;
   end JSON;

   --------------
   --  Length  --
   --------------

   function Length
     (Self : in Organization_List_Object)
      return Natural
   is
   begin
      return Natural (Self.Org_List.Length);
   end Length;

   ----------------------------------
   --  Organization_Element_Basic  --
   ----------------------------------

   function Organization_Element
     (C : in out Cursor)
      return Organization_Object'Class
   is
      use Common;

      O : Organization_Object;
   begin
      O := Organization_Object'
        (Cntacts   => Contacts.Null_Contact_List_Object,
         Full_Name  => U (C.Value (0)),
         Identifier => U (C.Value (1)),
         JSON       => C.Json_Object_Value (2),
         O_Id       => Organization_Identifier (C.Integer_Value (3)));

      return O;
   end Organization_Element;

   -----------------------
   --  Organization_Id  --
   -----------------------

   function Organization_Id
     (Self : in Organization_Object)
      return Organization_Identifier
   is
   begin
      return Self.O_Id;
   end Organization_Id;

   ----------------------
   --  To_JSON_String  --
   ----------------------

   function To_JSON_String
     (Self      : in Organization_List_Object;
      View_Mode : in View.Mode := View.Full)
      return Common.JSON_String
   is
   begin
      return View.Organization.To_JSON (Self, View_Mode);
   end To_JSON_String;

   ----------------------
   --  To_JSON_String  --
   ----------------------

   function To_JSON_String
     (Self      : in Organization_Object;
      View_Mode : in View.Mode := View.Full)
      return Common.JSON_String
   is
   begin
      return View.Organization.To_JSON (Self, View_Mode);
   end To_JSON_String;

end Model.Organizations;
