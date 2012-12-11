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

with Ada.Strings.Unbounded;
with Common;
with GNATCOLL.JSON;
with Model.Contact;
with Model.Contacts;

package Model.Organization is

   type Organization_Object is tagged private;
   Null_Organization : constant Organization_Object;

   type Data_Mode is (Mini, Midi, Maxi);
   --  Mini: As plain as possible. No JSON document, no contacts.
   --  Midi: The organization JSON document is also fetched.
   --  Maxi: Midi + all contacts for the organization.

   procedure Add_Contact
     (Instance : in out Organization_Object;
      Contact  : in     Model.Contact.Object);
   --  Add Contact to Instance.

   function Contact_List
     (Self : in Organization_Object)
      return Model.Contacts.List;
   --  Return all the contacts associated with the Self organization.

   function Create
     (ID         : in Organization_Identifier;
      Full_Name  : in String;
      Identifier : in String;
      JSON       : in GNATCOLL.JSON.JSON_Value;
      Mode       : in Data_Mode := Mini)
      return Organization_Object;

   function Full_Name
     (Self : in Organization_Object)
      return String;
   --  Return the full name of the Self organization.

   function Get
     (ID   : in Organization_Identifier;
      Mode : in Data_Mode := Mini)
      return Organization_Object;
   --  Get the organization that match ID. This does NOT fetch the contacts
   --  that belong to the O_ID organization.

   function ID
     (Self : in Organization_Object)
      return Organization_Identifier;
   --  Return the Organization_ID for the Self organization.

   function Identifier
     (Self : in Organization_Object)
      return String;
   --  Return the unique string identifier for the Self organization.

   function JSON
     (Self : in Organization_Object)
      return GNATCOLL.JSON.JSON_Value;
   --  Return the JSON document for the Self organization.

   function Mode
     (Instance : in Organization_Object)
      return Data_Mode;
   --  Return the data mode for Instance.

   function To_JSON_String
     (Self : in Organization_Object)
      return Common.JSON_String;
   --  Convert Organization to a JSON string. This call is convenient wrapper
   --  for the View.Organization.To_JSON function.

private

   use Ada.Strings.Unbounded;

   type Organization_Object is tagged
      record
         C_List     : Contacts.List;
         Full_Name  : Unbounded_String := Null_Unbounded_String;
         Identifier : Unbounded_String := Null_Unbounded_String;
         JSON       : GNATCOLL.JSON.JSON_Value := GNATCOLL.JSON.JSON_Null;
         Mode       : Data_Mode;
         O_ID       : Organization_Identifier := 0;
      end record;

   Null_Organization : constant Organization_Object
     := (C_List     => Contacts.Null_List,
         Full_Name  => Null_Unbounded_String,
         Identifier => Null_Unbounded_String,
         JSON       => GNATCOLL.JSON.JSON_Null,
         Mode       => Mini,
         O_ID       => 0);

end Model.Organization;
