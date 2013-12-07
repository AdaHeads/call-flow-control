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

with Common;
with Model.Attribute;
with Model.Attributes;

private with Ada.Strings.Unbounded;

package Model.Message is

   type Object is tagged private;
   Null_Object : constant Object;

   procedure Add_Attribute
     (Instance  : in out Object;
      Attribute : in     Model.Attribute.Object)
   with Pre => Instance.ID = Attribute.Contact_ID;
   --  Add Attribute to Instance.

   function Attributes
     (Instance : in Object)
      return Model.Attributes.List;
   --  Return the Attribute objects associated with Instance.

   function ID
     (Instance : in Object)
      return Contact_Identifier;
   --  Return the ID of Instance.

   function Create
     (ID        : in Contact_Identifier;
      Full_Name : in String;
      Is_Human  : in Boolean)
      return Object;
   --  Create a contact object.

   function Full_Name
     (Instance : in Object)
      return String;
   --  Return the full name of Instance.

   function Get
     (ID : in Contact_Identifier)
      return Object;
   --  Get the contact that match ID, complete with all the attributes that may
   --  be associated with the contact.

   function Get
     (ID : in Organization_Contact_Identifier)
      return Object;
   --  Return the contact that match ID, complete with all the attributes that
   --  may be associated with the contact.

   function Is_Human
     (Instance : in Object)
      return Boolean;
   --  Return whether or not Instance is human.

   function To_JSON_String
     (Instance : in Object)
      return Common.JSON_String;
   --  Convert Instance to a JSON string. This call is convenient wrapper for
   --  the View.Contact.To_JSON function.

private

   use Ada.Strings.Unbounded;

   type Object is tagged
      record
         Attributes : Model.Attributes.List := Model.Attributes.Null_List;
         ID         : Contact_Identifier := 0;
         Full_Name  : Unbounded_String := Null_Unbounded_String;
         Is_Human   : Boolean := True;
      end record;

   Null_Object : constant Object
     := (Attributes => Model.Attributes.Null_List,
         ID         => 0,
         Full_Name  => Null_Unbounded_String,
         Is_Human   => True);

end Model.Message;
