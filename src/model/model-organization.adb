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
     (Cursor : in out Database_Cursor'Class)
      return Object;
   --  Transforms the low level index based Cursor into an organization object.

   function Organization_Midi_Element
     (C : in out Database_Cursor'Class)
      return Object;
   --  Transforms the low level index based Cursor into an organization object.

   function Organization_Mini_Element
     (C : in out Database_Cursor'Class)
      return Object;
   --  Transforms the low level index based Cursor into an organization object.

   procedure Process_Select_Query_Midi is new Storage.Process_Select_Query
     (Database_Cursor   => Database_Cursor,
      Element           => Object,
      Cursor_To_Element => Organization_Midi_Element);

   procedure Process_Select_Query_Mini is new Storage.Process_Select_Query
     (Database_Cursor   => Database_Cursor,
      Element           => Object,
      Cursor_To_Element => Organization_Mini_Element);

   procedure Process_Select_Query_Maxi is new Storage.Process_Select_Query
     (Database_Cursor   => Database_Cursor,
      Element           => Object,
      Cursor_To_Element => Organization_Maxi_Element);

   -------------------
   --  Add_Contact  --
   -------------------

   procedure Add_Contact
     (Instance : in out Object;
      Contact  : in     Model.Contact.Object)
   is
   begin
      Instance.Contact_List.Add_Contact (Contact, Instance.ID);
   end Add_Contact;

   --------------------
   --  Contact_List  --
   --------------------

   function Contact_List
     (Instance : in Object)
      return Model.Contacts.List
   is
   begin
      return Instance.Contact_List;
   end Contact_List;

   ---------------
   --  Create   --
   ---------------

   function Create
     (ID         : in Organization_Identifier;
      Full_Name  : in String;
      Identifier : in String;
      JSON       : in GNATCOLL.JSON.JSON_Value;
      Mode       : in Data_Mode := Request_Parameters.Mini)
      return Object
   is
      use Common;
   begin
      return (Contact_List => Model.Contacts.Null_List,
              Full_Name    => U (Full_Name),
              Identifier   => U (Identifier),
              JSON         => JSON,
              Mode         => Mode,
              ID           => ID);
   end Create;

   -----------------
   --  Full_Name  --
   -----------------

   function Full_Name
     (Instance : in Object)
      return String
   is
   begin
      return To_String (Instance.Full_Name);
   end Full_Name;

   -----------
   --  Get  --
   -----------

   function Get
     (ID   : in Organization_Identifier;
      Mode : in Data_Mode := Request_Parameters.Mini)
      return Object
   is
      use GNATCOLL.SQL.Exec;

      procedure Get_Element
        (Instance : in Object);

      Organization : Object;

      -------------------
      --  Get_Element  --
      -------------------

      procedure Get_Element
        (Instance : in Object)
      is
      begin
         Organization := Instance;
      end Get_Element;

      Parameters : constant SQL_Parameters := (1 => +Integer (ID));
   begin
      case Mode is
         when Request_Parameters.Mini =>
            Process_Select_Query_Mini
              (Process_Element    => Get_Element'Access,
               Prepared_Statement => SQL.Organization_Mini_Prepared,
               Query_Parameters   => Parameters);
         when Request_Parameters.Midi =>
            Process_Select_Query_Midi
              (Process_Element    => Get_Element'Access,
               Prepared_Statement => SQL.Organization_Midi_Prepared,
               Query_Parameters   => Parameters);
         when Request_Parameters.Maxi =>
            Process_Select_Query_Maxi
              (Process_Element    => Get_Element'Access,
               Prepared_Statement => SQL.Organization_Maxi_Prepared,
               Query_Parameters   => Parameters);
      end case;

      return Organization;
   end Get;

   ----------
   --  ID  --
   ----------

   function ID
     (Instance : in Object)
      return Organization_Identifier
   is
   begin
      return Instance.ID;
   end ID;

   ------------------
   --  Identifier  --
   ------------------

   function Identifier
     (Instance : in Object)
      return String
   is
   begin
      return To_String (Instance.Identifier);
   end Identifier;

   ------------
   --  JSON  --
   ------------

   function JSON
     (Instance : in Object)
      return GNATCOLL.JSON.JSON_Value
   is
   begin
      return Instance.JSON;
   end JSON;

   ------------
   --  Mode  --
   ------------

   function Mode
     (Instance : in Object)
      return Data_Mode
   is
   begin
      return Instance.Mode;
   end Mode;

   ---------------------------------
   --  Organization_Maxi_Element  --
   ---------------------------------

   function Organization_Maxi_Element
     (Cursor : in out Database_Cursor'Class)
      return Object
   is
      use Common;

      Contact      : Model.Contact.Object;
      Organization : Object;
   begin
      Organization :=
        (Contact_List => Contacts.Null_List,
         Full_Name    => U (Cursor.Value (0)),
         ID           => Organization_Identifier (Cursor.Integer_Value (3)),
         Identifier   => U (Cursor.Value (1)),
         JSON         => Cursor.Json_Object_Value (2),
         Mode         => Request_Parameters.Maxi);

      while Cursor.Has_Row loop
         if not Cursor.Is_Null (4) then
            Contact := Model.Contact.Create
              (ID        => Contact_Identifier
                 (Cursor.Integer_Value (4, Default => 0)),
               Full_Name => Cursor.Value (5),
               Is_Human  => Cursor.Boolean_Value (6));

            Contact.Add_Attribute
              (Model.Attribute.Create
                 (ID   => Attribute_Identifier'
                    (CID => Contact.ID, OID => Organization.ID),
                  JSON => Cursor.Json_Object_Value (7)));

            Organization.Contact_List.Add_Contact (Contact => Contact,
                                                   ID      => Organization.ID);

         end if;

         Cursor.Next;
      end loop;

      return Organization;
   end Organization_Maxi_Element;

   ---------------------------------
   --  Organization_Midi_Element  --
   ---------------------------------

   function Organization_Midi_Element
     (C : in out Database_Cursor'Class)
      return Object
   is
      use Common;
   begin
      return (Contact_List => Contacts.Null_List,
              Full_Name    => U (C.Value (0)),
              ID           => Organization_Identifier (C.Integer_Value (3)),
              Identifier   => U (C.Value (1)),
              JSON         => C.Json_Object_Value (2),
              Mode         => Request_Parameters.Midi);
   end Organization_Midi_Element;

   ---------------------------------
   --  Organization_Mini_Element  --
   ---------------------------------

   function Organization_Mini_Element
     (C : in out Database_Cursor'Class)
      return Object
   is
      use Common;
   begin
      return (Contact_List => Contacts.Null_List,
              Full_Name    => U (C.Value (0)),
              ID           => Organization_Identifier (C.Integer_Value (2)),
              Identifier   => U (C.Value (1)),
              JSON         => GNATCOLL.JSON.JSON_Null,
              Mode         => Request_Parameters.Mini);
   end Organization_Mini_Element;

   ----------------------
   --  To_JSON_String  --
   ----------------------

   function To_JSON_String
     (Instance : in Object)
      return Common.JSON_String
   is
   begin
      return View.Organization.To_JSON_String (Instance);
   end To_JSON_String;

end Model.Organization;
