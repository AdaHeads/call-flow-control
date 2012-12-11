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

with Model.Attribute;
with SQL_Statements.Organization;
with Storage;
with View.Organization;

package body Model.Organization is

   package SQL renames SQL_Statements.Organization;

   function Organization_Maxi_Element
     (C : in out Database_Cursor'Class)
      return Organization_Object;

   function Organization_Midi_Element
     (C : in out Database_Cursor'Class)
      return Organization_Object;

   function Organization_Mini_Element
     (C : in out Database_Cursor'Class)
      return Organization_Object;

   procedure Process_Select_Query_Midi is new Storage.Process_Select_Query
     (Database_Cursor   => Database_Cursor,
      Element           => Organization_Object,
      Cursor_To_Element => Organization_Midi_Element);

   procedure Process_Select_Query_Mini is new Storage.Process_Select_Query
     (Database_Cursor   => Database_Cursor,
      Element           => Organization_Object,
      Cursor_To_Element => Organization_Mini_Element);
   --  TODO: Fix SQL so we don't fetch the unnecessary JSON column.

   procedure Process_Select_Query_Maxi is new Storage.Process_Select_Query
     (Database_Cursor   => Database_Cursor,
      Element           => Organization_Object,
      Cursor_To_Element => Organization_Maxi_Element);

   -------------------
   --  Add_Contact  --
   -------------------

   procedure Add_Contact
     (Instance : in out Organization_Object;
      Contact  : in     Model.Contact.Object)
   is
   begin
      Instance.C_List.Add_Contact (Contact, Instance.O_ID);
   end Add_Contact;

   --------------------
   --  Contact_List  --
   --------------------

   function Contact_List
     (Self : in Organization_Object)
      return Model.Contacts.List
   is
   begin
      return Self.C_List;
   end Contact_List;

   ---------------
   --  Create   --
   ---------------

   function Create
     (ID         : in Organization_Identifier;
      Full_Name  : in String;
      Identifier : in String;
      JSON       : in GNATCOLL.JSON.JSON_Value;
      Mode       : in Data_Mode := Mini)
      return Organization_Object
   is
      use Common;

   begin
      return (C_List     => Model.Contacts.Null_List,
              Full_Name  => U (Full_Name),
              Identifier => U (Identifier),
              JSON       => JSON,
              Mode       => Mode,
              O_ID       => ID);
   end Create;

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

   function Get
     (ID   : in Organization_Identifier;
      Mode : in Data_Mode := Mini)
      return Organization_Object
   is
      use GNATCOLL.SQL.Exec;

      procedure Get_Element
        (Organization : in Organization_Object);

      O : Organization_Object;

      -------------------
      --  Get_Element  --
      -------------------

      procedure Get_Element
        (Organization : in Organization_Object)
      is
      begin
         O := Organization;
      end Get_Element;

      Parameters : constant SQL_Parameters := (1 => +Integer (ID));
   begin
      case Mode is
         when Mini =>
            Process_Select_Query_Mini
              (Process_Element    => Get_Element'Access,
               Prepared_Statement => SQL.Organization_Mini_Prepared,
               Query_Parameters   => Parameters);
         when Midi =>
            Process_Select_Query_Midi
              (Process_Element    => Get_Element'Access,
               Prepared_Statement => SQL.Organization_Midi_Prepared,
               Query_Parameters   => Parameters);
         when Maxi =>
            Process_Select_Query_Maxi
              (Process_Element    => Get_Element'Access,
               Prepared_Statement => SQL.Organization_Maxi_Prepared,
               Query_Parameters   => Parameters);
      end case;

      return O;
   end Get;

   ----------
   --  ID  --
   ----------

   function ID
     (Self : in Organization_Object)
      return Organization_Identifier
   is
   begin
      return Self.O_ID;
   end ID;

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

   ------------
   --  Mode  --
   ------------

   function Mode
     (Instance : in Organization_Object)
      return Data_Mode
   is
   begin
      return Instance.Mode;
   end Mode;

   ---------------------------------
   --  Organization_Maxi_Element  --
   ---------------------------------

   function Organization_Maxi_Element
     (C : in out Database_Cursor'Class)
      return Organization_Object
   is
      use Common;
      use Model.Contact;
      --  use Model.Attributes;
      use type GNATCOLL.SQL.Exec.Field_Index;

      Contact : Model.Contact.Object;

      O : Organization_Object;
   begin
      O := (C_List     => Contacts.Null_List,
            Full_Name  => U (C.Value (0)),
            Identifier => U (C.Value (1)),
            JSON       => C.Json_Object_Value (2),
            Mode       => Maxi,
            O_ID       => Organization_Identifier (C.Integer_Value (3)));

      if C.Field_Count > 4 then
         --  This is a full organization, complete with contacts.
         while C.Has_Row loop
            if not C.Is_Null (4) then
               Contact := Create
                 (ID      => Contact_Identifier
                    (C.Integer_Value (4, Default => 0)),
                  Full_Name => C.Value (5),
                  Is_Human  => C.Boolean_Value (6));

               Contact.Add_Attribute
                 (Model.Attribute.Create (ID   => Attribute_Identifier'
                            (CID => Contact.ID, OID => O.O_ID),
                          JSON => C.Json_Object_Value (7)));

               O.C_List.Add_Contact (Contact => Contact,
                                     ID      => O.O_ID);

            end if;

            C.Next;
         end loop;
      end if;

      return O;
   end Organization_Maxi_Element;

   ---------------------------------
   --  Organization_Midi_Element  --
   ---------------------------------

   function Organization_Midi_Element
     (C : in out Database_Cursor'Class)
      return Organization_Object
   is
      use Common;
   begin
      return (C_List     => Contacts.Null_List,
              Full_Name  => U (C.Value (0)),
              Identifier => U (C.Value (1)),
              JSON       => C.Json_Object_Value (2),
              Mode       => Midi,
              O_ID       => Organization_Identifier (C.Integer_Value (3)));
   end Organization_Midi_Element;

   ---------------------------------
   --  Organization_Mini_Element  --
   ---------------------------------

   function Organization_Mini_Element
     (C : in out Database_Cursor'Class)
      return Organization_Object
   is
      use Common;
   begin
      return (C_List     => Contacts.Null_List,
              Full_Name  => U (C.Value (0)),
              Identifier => U (C.Value (1)),
              JSON       => GNATCOLL.JSON.JSON_Null,
              Mode       => Mini,
              O_ID       => Organization_Identifier (C.Integer_Value (2)));
   end Organization_Mini_Element;

   ----------------------
   --  To_JSON_String  --
   ----------------------

   function To_JSON_String
     (Self : in Organization_Object)
      return Common.JSON_String
   is
   begin
      return View.Organization.To_JSON_String (Self);
   end To_JSON_String;

end Model.Organization;
