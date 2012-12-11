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

   type Object is tagged private;
   Null_Organization : constant Object;

   type Data_Mode is (Mini, Midi, Maxi);
   --  Mini: As plain as possible. No JSON document, no contacts.
   --  Midi: Mini + the organization JSON document is also fetched.
   --  Maxi: Midi + all contacts for the organization.

   procedure Add_Contact
     (Instance : in out Object;
      Contact  : in     Model.Contact.Object);
   --  Add Contact to Instance.

   function Contact_List
     (Instance : in Object)
      return Model.Contacts.List;
   --  Return all the contacts associated with Instance.

   function Create
     (ID         : in Organization_Identifier;
      Full_Name  : in String;
      Identifier : in String;
      JSON       : in GNATCOLL.JSON.JSON_Value;
      Mode       : in Data_Mode := Mini)
      return Object;
   --  Create an organization object.

   function Full_Name
     (Instance : in Object)
      return String;
   --  Return the full name of Instance.

   function Get
     (ID   : in Organization_Identifier;
      Mode : in Data_Mode := Mini)
      return Object;
   --  Get the organization that match ID. The amount of data fetched depends
   --  on the Mode parameter. See comment for the Data_Mode type.

   function ID
     (Instance : in Object)
      return Organization_Identifier;
   --  Return the ID for Instance.

   function Identifier
     (Instance : in Object)
      return String;
   --  Return the unique string identifier for Instance.

   function JSON
     (Instance : in Object)
      return GNATCOLL.JSON.JSON_Value;
   --  Return the JSON object for Instance.

   function Mode
     (Instance : in Object)
      return Data_Mode;
   --  Return the data mode for Instance.

   function To_JSON_String
     (Instance : in Object)
      return Common.JSON_String;
   --  Convert Instance to a JSON string. This call is convenient wrapper
   --  for the View.Organization.To_JSON_String function.

private

   use Ada.Strings.Unbounded;

   type Object is tagged
      record
         Contact_List : Contacts.List;
         Full_Name    : Unbounded_String := Null_Unbounded_String;
         ID           : Organization_Identifier := 0;
         Identifier   : Unbounded_String := Null_Unbounded_String;
         JSON         : GNATCOLL.JSON.JSON_Value := GNATCOLL.JSON.JSON_Null;
         Mode         : Data_Mode;
      end record;

   Null_Organization : constant Object
     := (Contact_List => Contacts.Null_List,
         Full_Name    => Null_Unbounded_String,
         ID           => 0,
         Identifier   => Null_Unbounded_String,
         JSON         => GNATCOLL.JSON.JSON_Null,
         Mode         => Mini);

end Model.Organization;
