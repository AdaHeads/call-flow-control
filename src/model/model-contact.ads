-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                              Model.Contact                                --
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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;
with Common;
with Model.Contact_Attributes;

package Model.Contact is

   type Contact_Object is tagged private;
   Null_Contact_Object : constant Contact_Object;

   function Equal
     (Left, Right : in Model.Contact_Attributes.Contact_Attributes_Object)
      return Boolean;

   package Attributes_List is new Ada.Containers.Doubly_Linked_Lists
     (Model.Contact_Attributes.Contact_Attributes_Object,
      Equal);

   function Get
     (Id : in Contact_Id)
      return Contact_Object;
   --  Return the contact that match Id, complete with all the attributes that
   --  may be associated with the contact.

   procedure Get
     (Id      : in Contact_Id;
      Process : not null access
        procedure (Element : in Contact_Object'Class));
   --  For every contact with Id in the database, a Contact_Object is handed to
   --  Process.

   function Get_Attributes
     (Contact : in Contact_Object)
      return Attributes_List.List;
   --  Return the a linked list of Contact_Attributes_Object's associated with
   --  Contact.

   function Get_Full_Name
     (Contact : in Contact_Object)
      return String;
   --  Return the full name of the Contact.

   function Get_Contact_Id
     (Contact : in Contact_Object)
      return Contact_Id;
   --  Return the id of the Contact.

   function Get_Is_Human
     (Contact : in Contact_Object)
      return Boolean;
   --  Return whether or not the Contact is human.

   function To_JSON
     (Contact : in Contact_Object)
      return Common.JSON_String;
   --  Convert the Contact to a JSON string. This call is convenient wrapper
   --  for the View.Contact.To_JSON function.

private

   use Ada.Strings.Unbounded;

   type Contact_Object is tagged
      record
         Attr_List   : Attributes_List.List;
         C_Id        : Contact_Id := 0;
         Full_Name   : Unbounded_String := Null_Unbounded_String;
         Is_Human    : Boolean := True;
      end record;

   Null_Contact_Object : constant Contact_Object
     := (Attr_List   => Attributes_List.Empty_List,
         C_Id        => 0,
         Full_Name   => Null_Unbounded_String,
         Is_Human    => True);

   function Contact_Element
     (C : in out Cursor)
      return Contact_Object'Class;
   --  Transforms the low level index based Cursor into the more readable
   --  Contact_Object record.

end Model.Contact;
