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

with SQL_Statements.Attribute;
with Storage;
with View.Attribute;

package body Model.Attribute is

   use GNATCOLL.SQL;
   use GNATCOLL.SQL.Exec;

   package SQL renames SQL_Statements.Attribute;

   function Contact_Attributes_Element
     (C : in out Database_Cursor'Class)
      return Object;
   --  Transforms the low level index based Cursor into the more readable
   --  Contact_Attributes_Object record.

   procedure Process_Select_Query is new Storage.Process_Select_Query
     (Database_Cursor   => Database_Cursor,
      Element           => Object,
      Cursor_To_Element => Contact_Attributes_Element);

   procedure For_Each
     (ID      : in Attribute_ID;
      Process : not null access
        procedure (Element : in Object));
   --  For every contact attribute row with ID in the database, an attribute
   --  object is handed to Process.

   ----------------------------------
   --  Contact_Attributes_Element  --
   ----------------------------------

   function Contact_Attributes_Element
     (C : in out Database_Cursor'Class)
      return Object
   is
   begin
      return
        (ID   => Attribute_ID'
           (CID => Contact_Identifier (C.Integer_Value (0, Default => 0)),
            OID => Organization_Identifier
              (C.Integer_Value (1, Default => 0))),
         JSON => C.Json_Object_Value (2));
   end Contact_Attributes_Element;

   ------------------
   --  Contact_ID  --
   ------------------

   function Contact_ID
     (Instance : in Object)
      return Contact_Identifier
   is
   begin
      return Instance.ID.CID;
   end Contact_ID;

   --------------
   --  Create  --
   --------------

   function Create
     (ID   : in Attribute_ID;
      JSON : in GNATCOLL.JSON.JSON_Value)
      return Object
   is
   begin
      return (ID   => Attribute_ID'(CID => ID.CID, OID => ID.OID),
              JSON => JSON);
   end Create;

   ----------------
   --  For_Each  --
   ----------------

   procedure For_Each
     (ID      : in Attribute_ID;
      Process : not null access
        procedure (Element : in Object))
   is
      Parameters : constant SQL_Parameters := (1 => +Integer (ID.CID),
                                               2 => +Integer (ID.OID));
   begin
      Process_Select_Query
        (Process_Element    => Process,
         Prepared_Statement => SQL.Contact_Organization_Attributes_Prepared,
         Query_Parameters   => Parameters);
   end For_Each;

   -----------
   --  Get  --
   -----------

   function Get
     (ID : in Attribute_ID)
      return Object
   is
      procedure Get_Element
        (Contact_Attributes : in Object);

      CA : Object;

      -------------------
      --  Get_Element  --
      -------------------

      procedure Get_Element
        (Contact_Attributes : in Object)
      is
      begin
         CA := Contact_Attributes;
      end Get_Element;
   begin
      For_Each (ID, Get_Element'Access);
      return CA;
   end Get;

   ----------
   --  ID  --
   ----------

   function ID
     (Instance : in Object)
      return Attribute_ID
   is
   begin
      return Instance.ID;
   end ID;

   ------------
   --  JSON  --
   ------------

   function JSON
     (Instance : Object)
      return GNATCOLL.JSON.JSON_Value
   is
   begin
      return Instance.JSON;
   end JSON;

   -----------------------
   --  Org_ID  --
   -----------------------

   function Organization_ID
     (Instance : Object)
      return Organization_Identifier
   is
   begin
      return Instance.ID.OID;
   end Organization_ID;

   ----------------------
   --  To_JSON_String  --
   ----------------------

   function To_JSON_String
     (Instance : in Object)
      return Common.JSON_String
   is
   begin
      return View.Attribute.To_JSON_String (Instance);
   end To_JSON_String;

end Model.Attribute;
