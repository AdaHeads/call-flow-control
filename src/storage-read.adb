-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                               Storage.Read                                --
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

with Database;
with Errors;
with GNATCOLL.JSON;
with Yolk.Log;
with Yolk.Utilities;

package body Storage.Read is

   ---------------
   --  Contact  --
   ---------------

   function Contact
     (Ce_Id  : in Positive)
      return String
   is
      use Database;
      use Errors;
      use GNATCOLL.JSON;
      use Yolk.Log;
      use Yolk.Utilities;

      Contact_JSON   : JSON_Value := Create_Object;
      Cursor         : Exec.Forward_Cursor;
      DB_Connections : Database_Connection_Pool;
      Query          : constant SQL_Query :=
                         SQL_Select (Fields        =>
                                       Contactentity.Ce_Id &
                                       Contactentity.Ce_Name &
                                       Contactentity.Json,
                                     Where         =>
                                       Contactentity.Ce_Id = Ce_Id,
                                     Auto_Complete => True);

      Valid          : Boolean := False;
      Value          : Unbounded_String := Null_Unbounded_String;
   begin
      Contact_Cache.Read (Key      => Positive'Image (Ce_Id),
                          Is_Valid => Valid,
                          Value    => Value);

      if not Valid then
         DB_Connections := Get_DB_Connections;

         Fetch_Data :
         for k in DB_Connections.Hosts'Range loop
            Cursor.Fetch (DB_Connections.Hosts (k), Query);

            if DB_Connections.Hosts (k).Success then
               while Exec.Has_Row (Cursor) loop
                  Contact_JSON := GNATCOLL.JSON.Read
                    (Cursor.Value (2),
                     "json.error");

                  Contact_JSON.Set_Field
                    (Field_Name => Cursor.Field_Name (0),
                     Field      => Cursor.Integer_Value (0));

                  Contact_JSON.Set_Field
                    (Field_Name => Cursor.Field_Name (1),
                     Field      => Cursor.Value (1));

                  Cursor.Next;
               end loop;

               Value := TUS (Write (Contact_JSON));
               Contact_Cache.Write (Key   => Positive'Image (Ce_Id),
                                    Value => Value);
               --  TODO: Write code so we only cache objects with actual
               --  content. No need to cache empty JSON object strings.

               exit Fetch_Data;
            else
               DB_Connections.Status (k) := Failed;
               Register_Failed_DB_Connection (Pool => DB_Connections);

               Trace (Handle  => Info,
                      Message => "Cannot complete query to the " &
                      Database_Connection_Priority'Image (k) &
                      " database server. Message from server is '" &
                      Trim (Exec.Error (DB_Connections.Hosts (k))) & "'");

               if k = Database_Connection_Priority'Last then
                  raise Database_Error with Trim
                    (Exec.Error (DB_Connections.Hosts (k)));
               end if;
            end if;
         end loop Fetch_Data;
      end if;

      return TS (Value);
   exception
      when Event : Database_Error =>
         return Exception_Handler
           (Event   => Event,
            Message => "Fatal database error in Storage.Read.Contact " &
            "(" & Trim (Positive'Image (Ce_Id)) & ")");
      when Event : others =>
         return Exception_Handler
           (Event   => Event,
            Message => "Error in call to Storage.Read.Contact " &
            "(" & Trim (Positive'Image (Ce_Id)) & ")");
   end Contact;

   --------------------------
   --  Contact_Attributes  --
   --------------------------

   function Contact_Attributes
     (Ce_Id  : in Natural)
      return String
   is
   begin
      return "{CONTACT_ATTRIBUTES ce_id:" & Natural'Image (Ce_Id) & "}";
   end Contact_Attributes;

   --------------------
   --  Contact_Tags  --
   --------------------

   function Contact_Tags
     (Ce_Id  : in Natural)
      return String
   is
   begin
      return "{CONTACT_TAGS ce_id:" & Natural'Image (Ce_Id) & "}";
   end Contact_Tags;

   ----------------
   --  Contacts  --
   ----------------

   function Contacts
     (Org_Id : in Natural)
      return String
   is
   begin
      return "{CONTACTS org_id:" & Natural'Image (Org_Id) & "}";
   end Contacts;

   ---------------------------
   --  Contacts_Attributes  --
   ---------------------------

   function Contacts_Attributes
     (Org_Id : in Natural)
      return String
   is
   begin
      return "{CONTACTS_ATTRIBUTES org_id:" & Natural'Image (Org_Id) & "}";
   end Contacts_Attributes;

   ---------------------
   --  Contacts_Tags  --
   ---------------------

   function Contacts_Tags
     (Org_Id : in Natural)
      return String
   is
   begin
      return "{CONTACTS_TAGS org_id:" & Natural'Image (Org_Id) & "}";
   end Contacts_Tags;

   --------------------
   --  Organization  --
   --------------------

   function Organization
     (Org_Id : in Natural)
      return String
   is
   begin
      return "{ORGANIZATION org_id:" & Natural'Image (Org_Id) & "}";
   end Organization;

end Storage.Read;
