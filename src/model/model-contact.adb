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

with SQL_Statements.Contact;
with Storage;
with View.Contact;

package body Model.Contact is

   use GNATCOLL.SQL;
   use GNATCOLL.SQL.Exec;

   package SQL renames SQL_Statements.Contact;

   function Contact_Element
     (C : in out Database_Cursor'Class)
      return Object;
   --  Transforms the low level index based Cursor into ONE Contact_Object
   --  record.

   procedure Process_Select_Query is new Storage.Process_Select_Query
     (Database_Cursor   => Database_Cursor,
      Element           => Object,
      Cursor_To_Element => Contact_Element);

   procedure For_Each
     (ID      : in Contact_Identifier;
      Process : not null access procedure (Element : in Object));
   --  For every contact with ID in the database, a contact Object is handed to
   --  Process.

   procedure For_Each
     (CID     : in Contact_Identifier;
      OID     : in Organization_Identifier;
      Process : not null access procedure (Element : in Object));
   --  Hands a contact Object to Process for every contact in the database that
   --  match CID and belongs to OID.

   ---------------------
   --  Add_Attribute  --
   ---------------------

   procedure Add_Attribute
     (Instance  : in out Object;
      Attribute : in     Model.Attribute.Object)
   is
   begin
      if Attribute.Contact_ID = Instance.ID then
         Instance.Attributes.Add_Attribute (Attribute);
      end if;
   end Add_Attribute;

   ------------------
   --  Attributes  --
   ------------------

   function Attributes
     (Instance : in Object)
      return Model.Attributes.List
   is
   begin
      return Instance.Attributes;
   end Attributes;

   -----------------------
   --  Contact_Element  --
   -----------------------

   function Contact_Element
     (C : in out Database_Cursor'Class)
      return Object
   is
      use Common;

      CO : Object;
   begin
      CO := (Attributes =>
               Model.Attributes.Null_List,
             ID         =>
               Contact_Identifier (C.Integer_Value (0, Default => 0)),
             Full_Name  => U (C.Value (1)),
             Is_Human   => C.Boolean_Value (2));

      while C.Has_Row loop
         if not C.Is_Null (3) then
            CO.Attributes.Add_Attribute
              (Model.Attribute.Create
                 (ID   => Attribute_Identifier'
                    (CID => Contact_Identifier
                       (C.Integer_Value (4, Default => 0)),
                     OID => Organization_Identifier
                       (C.Integer_Value (5, Default => 0))),
                  JSON => C.Json_Object_Value (3)));
         end if;

         C.Next;
      end loop;

      return CO;
   end Contact_Element;

   --------------
   --  Create  --
   --------------

   function Create
     (ID        : in Contact_Identifier;
      Full_Name : in String;
      Is_Human  : in Boolean)
      return Object
   is
      use Common;
   begin
      return (Attributes =>
                Model.Attributes.Null_List,
              ID         => ID,
              Full_Name  => U (Full_Name),
              Is_Human   => Is_Human);
   end Create;

   ----------------
   --  For_Each  --
   ----------------

   procedure For_Each
     (ID      : in Contact_Identifier;
      Process : not null access procedure (Element : in Object))
   is
      Parameters : constant SQL_Parameters := (1 => +Integer (ID));
   begin
      Process_Select_Query
        (Process_Element    => Process,
         Prepared_Statement => SQL.Contact_Full_Prepared,
         Query_Parameters   => Parameters);
   end For_Each;

   ----------------
   --  For_Each  --
   ----------------

   procedure For_Each
     (CID     : in Contact_Identifier;
      OID     : in Organization_Identifier;
      Process : not null access procedure (Element : in Object))
   is
      Parameters : constant SQL_Parameters := (1 => +Integer (CID),
                                               2 => +Integer (OID));
   begin
      Process_Select_Query
        (Process_Element    => Process,
         Prepared_Statement => SQL.Contact_Org_Specified_Prepared,
         Query_Parameters   => Parameters);
   end For_Each;

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
     (ID : in Contact_Identifier)
      return Object
   is
      procedure Get_Element
        (Contact : in Object);

      C : Object := Null_Object;

      -------------------
      --  Get_Element  --
      -------------------

      procedure Get_Element
        (Contact : in Object)
      is
      begin
         C := Contact;
      end Get_Element;
   begin
      For_Each (ID, Get_Element'Access);
      return C;
   end Get;

   -----------
   --  Get  --
   -----------

   function Get
     (ID : in Organization_Contact_Identifier)
      return Object
   is
      procedure Get_Element
        (Contact : in Object);

      C : Object := Null_Object;

      -------------------
      --  Get_Element  --
      -------------------

      procedure Get_Element
        (Contact : in Object)
      is
      begin
         C := Contact;
      end Get_Element;
   begin
      For_Each (ID.CID, ID.OID, Get_Element'Access);
      return C;
   end Get;

   ----------
   --  ID  --
   ----------

   function ID
     (Instance : in Object)
      return Contact_Identifier
   is
   begin
      return Instance.ID;
   end ID;

   ----------------
   --  Is_Human  --
   ----------------

   function Is_Human
     (Instance : in Object)
      return Boolean
   is
   begin
      return Instance.Is_Human;
   end Is_Human;

   ---------------
   --  To_JSON  --
   ---------------

   function To_JSON
     (Instance : in Object)
      return Common.JSON_String
   is
   begin
      return View.Contact.To_JSON_String (Instance);
   end To_JSON;

end Model.Contact;
