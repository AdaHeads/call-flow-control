-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                           Model.Organizations                             --
--                                                                           --
--                                  SPEC                                     --
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

with Ada.Strings.Unbounded;
with Common;
with GNATCOLL.JSON;
with GNATCOLL.SQL.Exec;
with Model.Contacts;
with View;

package Model.Organizations is

   type Organization_Object is tagged private;
   Null_Organization : constant Organization_Object;

   type Organization_List_Object is tagged private;

   function Contact_List
     (Self : in Organization_Object)
      return Model.Contacts.Contact_List_Object;
   --  Return all the contacts associated with the Self organization.

   procedure For_Each
     (Self    : in Organization_List_Object;
      Process : not null access
        procedure (Element : in Organization_Object'Class));
   --  For every organization in the database, an Organization_Object is handed
   --  to process. These objects do NOT contain any contacts.

   procedure For_Each
     (O_ID    : in Organization_Identifier;
      Process : not null access
        procedure (Element : in Organization_Object'Class));
   --  For every organization with O_ID in the database, an Organization_Object
   --  is handed to Process. These organization objects do NOT contain any
   --  contacts.

   function Full_Name
     (Self : in Organization_Object)
      return String;
   --  Return the full name of the Self organization.

   procedure Get
     (Self : in out Organization_Object;
      O_ID : in Organization_Identifier);
   --  Get the organization that match O_ID. This does NOT fetch the contacts
   --  that belong to the O_ID organization.

   procedure Get_Full
     (Self : in out Organization_Object;
      O_ID : in     Organization_Identifier);
   --  Get the organization that match O_ID. This object contains ALL the
   --  contacts that are associated with the O_ID organization.

   function Identifier
     (Self : in Organization_Object)
      return String;
   --  Return the unique string identifier for the Self organization.

   function JSON
     (Self : in Organization_Object)
      return GNATCOLL.JSON.JSON_Value;
   --  Return the JSON document for the Self organization.

   function Organization_ID
     (Self : in Organization_Object)
      return Organization_Identifier;
   --  Return the Organization_Identifier for the Self organization.

   function To_JSON_String
     (Self      : in out Organization_List_Object;
      View_Mode : in     View.Mode)
      return Common.JSON_String;
   --  Convert Self into a JSON string. This call is convenient wrapper
   --  for the View.Organization.To_JSON function.
   --
   --  If View_Mode is View.Basic then the organization JSON documents are NOT
   --  added to the final JSON_String. This is handy in cases where only the
   --  id, identifier and full name of the organization is needed.
   --
   --  If View_Mode is View.Full, then the organization id, identifier and full
   --  name are added to the organization JSON document.
   --
   --  No matter the View_Mode, the organization contacts are NEVER added to
   --  the final JSON_String.

   function To_JSON_String
     (Self      : in Organization_Object;
      View_Mode : in View.Mode)
      return Common.JSON_String;
   --  Convert Organization to a JSON string. This call is convenient wrapper
   --  for the View.Organization.To_JSON function.
   --
   --  If View_Mode is View.Basic then the organization JSON document is NOT
   --  added to the final JSON_String. This is handy in cases where only the
   --  id, identifier and full name of the organization is needed.
   --
   --  If View_Mode is View.Full, then the organization id, identifier, full
   --  name and all the organization contacts are added to the organization
   --  JSON document.
   --
   --  Note that contacts only exist in Self if Get_Full has been called on the
   --  object.

private

   use Ada.Strings.Unbounded;

   type Cursor is new GNATCOLL.SQL.Exec.Forward_Cursor with null record;

   type Organization_Object is tagged
      record
         C_List     : Contacts.Contact_List_Object;
         Full_Name  : Unbounded_String := Null_Unbounded_String;
         Identifier : Unbounded_String := Null_Unbounded_String;
         JSON       : GNATCOLL.JSON.JSON_Value := GNATCOLL.JSON.JSON_Null;
         O_ID       : Organization_Identifier := 0;
      end record;

   Null_Organization : constant Organization_Object
     := (C_List     => Contacts.Null_Contact_List,
         Full_Name  => Null_Unbounded_String,
         Identifier => Null_Unbounded_String,
         JSON       => GNATCOLL.JSON.JSON_Null,
         O_ID       => 0);

   type Organization_List_Object is tagged null record;

   function Organization_Element
     (C : in out Cursor)
      return Organization_Object'Class;
   --  Transforms the low level index based Cursor into the more readable
   --  Organization_Object record. This one does NOT contain any contacts.

end Model.Organizations;
