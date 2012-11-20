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

with Model.Contact_Attributes;
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

   procedure For_Each_Full
     (O_Id    : in Organization_Identifier;
      Process : not null access
        procedure (Element : in Organization_Object'Class));
   --  For every organization with O_Id in the database, an Organization_Object
   --  is handed to Process. These organization objects do contain contacts.

   --------------------
   --  Contact_List  --
   --------------------

   function Contact_List
     (Self : in Organization_Object)
      return Model.Contacts.Contact_List_Object
   is
   begin
      return Self.C_List;
   end Contact_List;

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

   ----------------
   --  For_Each  --
   ----------------

   procedure For_Each
     (Self    : in Organization_List_Object;
      Process : not null access
        procedure (Element : in Organization_Object'Class))
   is
      pragma Unreferenced (Self);
   begin
      For_Each (Process);
   end For_Each;

   ---------------------
   --  For_Each_Full  --
   ---------------------

   procedure For_Each_Full
     (O_Id    : in Organization_Identifier;
      Process : not null access
        procedure (Element : in Organization_Object'Class))
   is
      use GNATCOLL.SQL.Exec;

      Parameters : constant SQL_Parameters := (1 => +Integer (O_Id));
   begin
      Fetch_Organization_Object
        (Process_Element    => Process,
         Prepared_Statement => SQL.Org_Contacts_Prepared,
         Query_Parameters   => Parameters);
   end For_Each_Full;

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
      For_Each_Full (O_Id, Get_Element'Access);
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

   ----------------------------------
   --  Organization_Element_Basic  --
   ----------------------------------

   function Organization_Element
     (C : in out Cursor)
      return Organization_Object'Class
   is
      use Common;
      use Model.Contacts;
      use Model.Contact_Attributes;
      use type GNATCOLL.SQL.Exec.Field_Index;

      Contact : Model.Contacts.Contact_Object;

      O : Organization_Object;
   begin
      O := Organization_Object'
        (C_List     => Contacts.Null_Contact_List,
         Full_Name  => U (C.Value (0)),
         Identifier => U (C.Value (1)),
         JSON       => C.Json_Object_Value (2),
         O_Id       => Organization_Identifier (C.Integer_Value (3)));

      if C.Field_Count > 4 then
         --  This is a full organization, complete with contacts.
         while C.Has_Row loop
            if not C.Is_Null (4) then
               Contact := Create
                 (C_Id      => Contact_Identifier
                    (C.Integer_Value (4, Default => 0)),
                  Full_Name => C.Value (5),
                  Is_Human  => C.Boolean_Value (6));

               Contact.Add_Attribute
                 (Create (Id   => Attributes_Identifier'
                            (C_Id => Contact.Contact_Id, O_Id => O.O_Id),
                          JSON => C.Json_Object_Value (7)));

               O.C_List.Add_Contact (Contact => Contact,
                                     O_Id    => O.O_Id);

            end if;

            C.Next;
         end loop;
      end if;

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
     (Self      : in out Organization_List_Object;
      View_Mode : in     View.Mode)
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
      View_Mode : in View.Mode)
      return Common.JSON_String
   is
   begin
      return View.Organization.To_JSON (Self, View_Mode);
   end To_JSON_String;

end Model.Organizations;
