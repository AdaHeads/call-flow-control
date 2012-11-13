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

with Model.Contacts_Attributes;
with SQL_Statements;
with Storage;
with View.Organization;

package body Model.Organizations is

   package SQL renames SQL_Statements;

   procedure Fetch_Basic_Organization_Object is new Storage.Process_Query
     (Database_Cursor   => Cursor,
      Element           => Organization_Object,
      Cursor_To_Element => Organization_Element_Basic);

   procedure Fetch_Full_Organization_Object is new Storage.Process_Query
     (Database_Cursor   => Cursor,
      Element           => Organization_Object,
      Cursor_To_Element => Organization_Element_Full);

   -------------------
   --  Add_Contact  --
   -------------------

   procedure Add_Contact
     (Organization : in out Organization_Object;
      Contact      : in     Model.Contacts.Contact_Object)
   is
   begin
      Organization.C_Map.Include (Contact.Contact_Id, Contact);
   end Add_Contact;

   ----------------
   --  Contacts  --
   ----------------

   function Contacts
     (Organization : in Organization_Object)
      return Contacts_Map.Map
   is
   begin
      return Organization.C_Map;
   end Contacts;

   -------------
   --  Equal  --
   -------------

   function Equal
     (Left, Right : in Model.Contacts.Contact_Object)
      return Boolean
   is
      use type Model.Contacts.Contact_Object;
   begin
      return Left = Right;
   end Equal;

   -----------------
   --  Full_Name  --
   -----------------

   function Full_Name
     (Organization : in Organization_Object)
      return String
   is
   begin
      return To_String (Organization.Full_Name);
   end Full_Name;

   -----------------
   --  Get_Basic  --
   -----------------

   procedure Get_Basic
     (O_Id    : in Organization_Identifier;
      Process : not null access
        procedure (Element : in Organization_Object'Class))
   is
      use GNATCOLL.SQL.Exec;

      Parameters : constant SQL_Parameters := (1 => +Integer (O_Id));
   begin
      Fetch_Basic_Organization_Object
        (Process_Element    => Process,
         Prepared_Statement => SQL.Prepared_Organization_Query,
         Query_Parameters   => Parameters);
      null;
   end Get_Basic;

   -----------------
   --  Get_Basic  --
   -----------------

   function Get_Basic
     (O_Id : in Organization_Identifier)
      return Organization_Object
   is
      procedure Get_Element
        (Organization : in Organization_Object'Class);

      O : Organization_Object := Null_Organization_Object;

      -------------------
      --  Get_Element  --
      -------------------

      procedure Get_Element
        (Organization : in Organization_Object'Class)
      is
      begin
         O := Organization_Object (Organization);
      end Get_Element;
   begin
      Get_Basic (O_Id, Get_Element'Access);
      return O;
   end Get_Basic;

   ----------------
   --  Get_Full  --
   ----------------

   procedure Get_Full
     (O_Id    : in Organization_Identifier;
      Process : not null access
        procedure (Element : in Organization_Object'Class))
   is
      use GNATCOLL.SQL.Exec;

      Parameters : constant SQL_Parameters := (1 => +Integer (O_Id));
   begin
      Fetch_Full_Organization_Object
        (Process_Element    => Process,
         Prepared_Statement => SQL.Prepared_Organization_Contacts_Query,
         Query_Parameters   => Parameters);
      null;
   end Get_Full;

   ----------------
   --  Get_Full  --
   ----------------

   function Get_Full
     (O_Id : in Organization_Identifier)
      return Organization_Object
   is
      procedure Get_Element
        (Organization : in Organization_Object'Class);

      O : Organization_Object := Null_Organization_Object;

      -------------------
      --  Get_Element  --
      -------------------

      procedure Get_Element
        (Organization : in Organization_Object'Class)
      is
      begin
         O := Organization_Object (Organization);
      end Get_Element;
   begin
      Get_Full (O_Id, Get_Element'Access);
      return O;
   end Get_Full;

   ------------------
   --  Identifier  --
   ------------------

   function Identifier
     (Organization : in Organization_Object)
      return String
   is
   begin
      return To_String (Organization.Identifier);
   end Identifier;

   ------------
   --  JSON  --
   ------------

   function JSON
     (Organization : in Organization_Object)
      return GNATCOLL.JSON.JSON_Value
   is
   begin
      return Organization.JSON;
   end JSON;

   ----------------------------------
   --  Organization_Element_Basic  --
   ----------------------------------

   function Organization_Element_Basic
     (C : in out Cursor)
      return Organization_Object'Class
   is
      use Common;

      O : Organization_Object;
   begin
      O := Organization_Object'
        (C_Map      => Contacts_Map.Empty_Map,
         Full_Name  => U (C.Value (0)),
         Identifier => U (C.Value (1)),
         JSON       => C.Json_Object_Value (2),
         O_Id       => Organization_Identifier (C.Integer_Value (3)));

      return O;
   end Organization_Element_Basic;

   ---------------------------------
   --  Organization_Element_Full  --
   ---------------------------------

   function Organization_Element_Full
     (C : in out Cursor)
      return Organization_Object'Class
   is
      use Common;

      CO : Model.Contacts.Contact_Object;
      O  : Organization_Object;
   begin
      O := Organization_Object'
        (C_Map      => Contacts_Map.Empty_Map,
         Full_Name  => U (C.Value (0)),
         Identifier => U (C.Value (1)),
         JSON       => C.Json_Object_Value (2),
         O_Id       => Organization_Identifier (C.Integer_Value (3)));

      while C.Has_Row loop
         if not C.Is_Null (4) then
            --  We have a contact
            CO := Model.Contacts.Create
              (C_Id      => Contact_Identifier (C.Integer_Value (4)),
               Full_Name => C.Value (5),
               Is_Human  => C.Boolean_Value (6));

            if not C.Is_Null (7) then
               --  We have contact attributes
               CO.Add_Attribute
                 (Model.Contacts_Attributes.Create
                    (C_Id => Contact_Identifier (C.Integer_Value (4)),
                     O_Id => Organization_Identifier (C.Integer_Value (3)),
                     JSON => C.Json_Object_Value (7)));
            end if;
         end if;

         O.Add_Contact (CO);

         C.Next;
      end loop;

      return O;
   end Organization_Element_Full;

   -----------------------
   --  Organization_Id  --
   -----------------------

   function Organization_Id
     (Organization : in Organization_Object)
      return Organization_Identifier
   is
   begin
      return Organization.O_Id;
   end Organization_Id;

   ---------------
   --  To_JSON  --
   ---------------

   function To_JSON
     (Organization : in Organization_Object)
      return Common.JSON_String
   is
   begin
      return View.Organization.To_JSON (Organization);
   end To_JSON;

end Model.Organizations;
