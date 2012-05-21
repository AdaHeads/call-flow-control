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

with AWS.Utils;
with Database;
with Errors;
with JSONIFY;
with Yolk.Utilities;

package body Storage.Read is

   procedure Failed_Query
     (Connection_Pool : in out Database_Connection_Pool;
      Connection_Type : in     Database_Connection_Type);
   --  If a query fails:
   --    1. Set the connection state to Failed.
   --    2. Raise the Database_Error exception if
   --         Connection_Type = Database_Connection_Type'Last
   --    3. If 2 is not True, Output a message to the Error log trace.

   function Get_Contact_From_DB
     (Ce_Id : in String)
      return Unbounded_String;
   --  Get the Ce_Id contact entity from persistent storage.

   function Get_Contact_Attributes_From_DB
     (Ce_Id  : in String)
      return Unbounded_String;
   --  Get the Ce_Id contact entity attributes from persistent storage.

   function Get_Contact_Full_From_DB
     (Ce_Id : in String)
      return Unbounded_String;
   --  Get the Ce_Id contact entity with Attributes from persistent storage.

   function Get_Org_Contacts_From_DB
     (Org_Id : in String)
      return Unbounded_String;
   --  Get the Org_Id contact entities from persistent storage.

   function Get_Org_Contacts_Attributes_From_DB
     (Org_Id : in String)
      return Unbounded_String;
   --  Get the Org_Id contact entity attributes from persistent storage.

   function Get_Org_Contacts_Full_From_DB
     (Org_Id : in String)
      return Unbounded_String;
   --  Get the Org_Id contact entities from persistent storage.

   function Get_Organization_From_DB
     (Org_Id : in String)
      return Unbounded_String;
   --  Get the Org_Id organization from persistent storage.

   --------------------
   --  Failed_Query  --
   --------------------

   procedure Failed_Query
     (Connection_Pool : in out Database_Connection_Pool;
      Connection_Type : in     Database_Connection_Type)
   is
      use Errors;

      Trimmed_DB_Error : constant String
        := Trim (Exec.Error (Connection_Pool (Connection_Type).Host));

      Connection       : constant String
        := Database_Connection_Type'Image (Connection_Type);

      Message : constant String :=  Trimmed_DB_Error &  "-" & Connection;
   begin
      Connection_Pool (Connection_Type).State := Failed;
      Register_Failed_DB_Connection (Pool => Connection_Pool);

      if Connection_Type = Database_Connection_Type'Last then
         raise Database_Error with Message;
      else
         Error_Handler (Message);
      end if;
   end Failed_Query;

   -------------------
   --  Get_Contact  --
   -------------------

   function Get_Contact
     (Ce_Id  : in String)
      return String
   is
      use AWS.Utils;
      use Errors;
      use Yolk.Utilities;

      Valid : Boolean          := False;
      Value : Unbounded_String := Null_Unbounded_String;
   begin
      Contact_Cache.Read (Key      => Ce_Id,
                          Is_Valid => Valid,
                          Value    => Value);

      if not Valid then
         if not Is_Number (Ce_Id) then
            raise GET_Parameter_Error with
              "ce_id must be a valid natural integer";
         end if;

         Value := Get_Contact_From_DB (Ce_Id);
      end if;

      return TS (Value);
   end Get_Contact;

   ------------------------------
   --  Get_Contact_Attributes  --
   ------------------------------

   function Get_Contact_Attributes
     (Ce_Id  : in String)
      return String
   is
      use AWS.Utils;
      use Errors;
      use Yolk.Utilities;

      Valid : Boolean          := False;
      Value : Unbounded_String := Null_Unbounded_String;
   begin
      Contact_Attributes_Cache.Read (Key      => Ce_Id,
                                     Is_Valid => Valid,
                                     Value    => Value);

      if not Valid then
         if not Is_Number (Ce_Id) then
            raise GET_Parameter_Error with
              "ce_id must be a valid natural integer";
         end if;

         Value := Get_Contact_Attributes_From_DB (Ce_Id);
      end if;

      return TS (Value);
   end Get_Contact_Attributes;

   --------------------------------------
   --  Get_Contact_Attributes_From_DB  --
   --------------------------------------

   function Get_Contact_Attributes_From_DB
     (Ce_Id : in String)
      return Unbounded_String
   is
      use Database;
      use Errors;

      Query : constant SQL_Query :=
                SQL_Select (Fields        =>
                              Contactentity_Attributes.Json &
                              Contactentity_Attributes.Ce_Id &
                              Contactentity_Attributes.Org_Id,
                            Where         =>
                              Contactentity_Attributes.Ce_Id =
                                Natural'Value (Ce_Id),
                            Auto_Complete => True);

      Cursor         : Exec.Forward_Cursor;
      DB_Connections : Database_Connection_Pool := Get_DB_Connections;
      Value          : Unbounded_String         := Null_Unbounded_String;
   begin
      Fetch_Data :
      for k in DB_Connections'Range loop
         Cursor.Fetch (DB_Connections (k).Host, Query);

         if DB_Connections (k).Host.Success then
            JSONIFY.Contact_Attributes (Cursor, Value);

            if Cursor.Processed_Rows > 0 then
               Contact_Attributes_Cache.Write (Key   => Ce_Id,
                                               Value => Value);
            end if;

            exit Fetch_Data;
         else
            Failed_Query (Connection_Pool => DB_Connections,
                          Connection_Type => k);
         end if;
      end loop Fetch_Data;

      return Value;
   end Get_Contact_Attributes_From_DB;

   ---------------------------
   --  Get_Contact_From_DB  --
   ---------------------------

   function Get_Contact_From_DB
     (Ce_Id : in String)
      return Unbounded_String
   is
      use Database;
      use Errors;

      Query : constant SQL_Query :=
                SQL_Select (Fields        =>
                              Contactentity.Json &
                              Contactentity.Ce_Id &
                              Contactentity.Ce_Name &
                              Contactentity.Is_Human,
                            Where         =>
                              Contactentity.Ce_Id = Natural'Value (Ce_Id),
                            Auto_Complete => True);

      Cursor         : Exec.Forward_Cursor;
      DB_Connections : Database_Connection_Pool := Get_DB_Connections;
      Value          : Unbounded_String         := Null_Unbounded_String;
   begin
      Fetch_Data :
      for k in DB_Connections'Range loop
         Cursor.Fetch (DB_Connections (k).Host, Query);

         if DB_Connections (k).Host.Success then
            JSONIFY.Contact (Cursor, Value);

            if Cursor.Processed_Rows > 0 then
               Contact_Cache.Write (Key   => Ce_Id,
                                    Value => Value);
            end if;

            exit Fetch_Data;
         else
            Failed_Query (Connection_Pool => DB_Connections,
                          Connection_Type => k);
         end if;
      end loop Fetch_Data;

      return Value;
   end Get_Contact_From_DB;

   ------------------------
   --  Get_Contact_Full  --
   ------------------------

   function Get_Contact_Full
     (Ce_Id  : in String)
      return String
   is
      use AWS.Utils;
      use Errors;
      use Yolk.Utilities;

      Valid : Boolean          := False;
      Value : Unbounded_String := Null_Unbounded_String;
   begin
      Contact_Full_Cache.Read (Key      => Ce_Id,
                               Is_Valid => Valid,
                               Value    => Value);

      if not Valid then
         if not Is_Number (Ce_Id) then
            raise GET_Parameter_Error with
              "ce_id must be a valid natural integer";
         end if;

         Value := Get_Contact_Full_From_DB (Ce_Id);
      end if;

      return TS (Value);
   end Get_Contact_Full;

   --------------------------------
   --  Get_Contact_Full_From_DB  --
   --------------------------------

   function Get_Contact_Full_From_DB
     (Ce_Id : in String)
      return Unbounded_String
   is
      use Database;
      use Errors;

      CA_Join : constant SQL_Left_Join_Table :=
                  Left_Join (Full    =>
                               Contactentity,
                             Partial =>
                               Contactentity_Attributes,
                             On      =>
                               Contactentity.Ce_Id =
                                 Contactentity_Attributes.Ce_Id);

      Query : constant SQL_Query :=
                SQL_Select (Fields        =>
                              Contactentity.Json &
                              Contactentity.Ce_Id &
                              Contactentity.Ce_Name &
                              Contactentity.Is_Human &
                              Contactentity_Attributes.Json &
                              Contactentity_Attributes.Org_Id &
                              Contactentity_Attributes.Ce_Id,
                            From          => CA_Join,
                            Where         =>
                              Contactentity.Ce_Id = Natural'Value (Ce_Id));

      Cursor         : Exec.Forward_Cursor;
      DB_Connections : Database_Connection_Pool := Get_DB_Connections;
      Value          : Unbounded_String         := Null_Unbounded_String;
   begin
      Fetch_Data :
      for k in DB_Connections'Range loop
         Cursor.Fetch (DB_Connections (k).Host, Query);

         if DB_Connections (k).Host.Success then
            JSONIFY.Contact_Full (Cursor, Value);

            if Cursor.Processed_Rows > 0 then
               Contact_Full_Cache.Write (Key   => Ce_Id,
                                      Value => Value);
            end if;

            exit Fetch_Data;
         else
            Failed_Query (Connection_Pool => DB_Connections,
                          Connection_Type => k);
         end if;
      end loop Fetch_Data;

      return Value;
   end Get_Contact_Full_From_DB;

   ------------------------
   --  Get_Org_Contacts  --
   ------------------------

   function Get_Org_Contacts
     (Org_Id : in String)
      return String
   is
      use AWS.Utils;
      use Errors;
      use Yolk.Utilities;

      Valid : Boolean          := False;
      Value : Unbounded_String := Null_Unbounded_String;
   begin
      Org_Contacts_Cache.Read (Key      => Org_Id,
                               Is_Valid => Valid,
                               Value    => Value);

      if not Valid then
         if not Is_Number (Org_Id) then
            raise GET_Parameter_Error with
              "org_id must be a valid natural integer";
         end if;

         Value := Get_Org_Contacts_From_DB (Org_Id);
      end if;

      return TS (Value);
   end Get_Org_Contacts;

   -----------------------------------
   --  Get_Org_Contacts_Attributes  --
   -----------------------------------

   function Get_Org_Contacts_Attributes
     (Org_Id : in String)
      return String
   is
      use AWS.Utils;
      use Errors;
      use Yolk.Utilities;

      Valid : Boolean          := False;
      Value : Unbounded_String := Null_Unbounded_String;
   begin
      Org_Contacts_Attributes_Cache.Read (Key      => Org_Id,
                                          Is_Valid => Valid,
                                          Value    => Value);

      if not Valid then
         if not Is_Number (Org_Id) then
            raise GET_Parameter_Error with
              "org_id must be a valid natural integer";
         end if;

         Value := Get_Org_Contacts_Attributes_From_DB (Org_Id);
      end if;

      return TS (Value);
   end Get_Org_Contacts_Attributes;

   -------------------------------------------
   --  Get_Org_Contacts_Attributes_From_DB  --
   -------------------------------------------

   function Get_Org_Contacts_Attributes_From_DB
     (Org_Id : in String)
      return Unbounded_String
   is
      use Database;
      use Errors;

      Query : constant SQL_Query :=
                SQL_Select (Fields        =>
                              Contactentity_Attributes.Json &
                              Contactentity_Attributes.Org_Id &
                              Contactentity_Attributes.Ce_Id,
                            Where         =>
                              Contactentity_Attributes.Org_Id =
                                Natural'Value (Org_Id),
                            Auto_Complete => True);

      Cursor         : Exec.Forward_Cursor;
      DB_Connections : Database_Connection_Pool := Get_DB_Connections;
      Value          : Unbounded_String         := Null_Unbounded_String;
   begin
      Fetch_Data :
      for k in DB_Connections'Range loop
         Cursor.Fetch (DB_Connections (k).Host, Query);

         if DB_Connections (k).Host.Success then
            JSONIFY.Org_Contacts_Attributes (Cursor, Value);

            if Cursor.Processed_Rows > 0 then
               Org_Contacts_Attributes_Cache.Write (Key   => Org_Id,
                                                    Value => Value);
            end if;

            exit Fetch_Data;
         else
            Failed_Query (Connection_Pool => DB_Connections,
                          Connection_Type => k);
         end if;
      end loop Fetch_Data;

      return Value;
   end Get_Org_Contacts_Attributes_From_DB;

   --------------------------------
   --  Get_Org_Contacts_From_DB  --
   --------------------------------

   function Get_Org_Contacts_From_DB
     (Org_Id : in String)
      return Unbounded_String
   is
      use Database;
      use Errors;

      Contacts : constant SQL_Left_Join_Table :=
                   Left_Join (Full    =>
                                Contactentity,
                              Partial =>
                                Organization_Contactentities,
                              On      =>
                                Contactentity.Ce_Id =
                                  Organization_Contactentities.Ce_Id);

      Query    : constant SQL_Query :=
                   SQL_Select (Fields =>
                                 Contactentity.Json &
                                 Contactentity.Ce_Id &
                                 Contactentity.Ce_Name &
                                 Contactentity.Is_Human,
                               From   => Contacts,
                               Where  =>
                                 Organization_Contactentities.Org_Id =
                                   Natural'Value (Org_Id));

      Cursor         : Exec.Forward_Cursor;
      DB_Connections : Database_Connection_Pool := Get_DB_Connections;
      Value          : Unbounded_String         := Null_Unbounded_String;
   begin
      Fetch_Data :
      for k in DB_Connections'Range loop
         Cursor.Fetch (DB_Connections (k).Host, Query);

         if DB_Connections (k).Host.Success then
            JSONIFY.Org_Contacts (Cursor, Value);

            if Cursor.Processed_Rows > 0 then
               Org_Contacts_Cache.Write (Key   => Org_Id,
                                         Value => Value);
            end if;

            exit Fetch_Data;
         else
            Failed_Query (Connection_Pool => DB_Connections,
                          Connection_Type => k);
         end if;
      end loop Fetch_Data;

      return Value;
   end Get_Org_Contacts_From_DB;

   -----------------------------
   --  Get_Org_Contacts_Full  --
   -----------------------------

   function Get_Org_Contacts_Full
     (Org_Id : in String)
      return String
   is
      use AWS.Utils;
      use Errors;
      use Yolk.Utilities;

      Valid : Boolean          := False;
      Value : Unbounded_String := Null_Unbounded_String;
   begin
      Org_Contacts_Full_Cache.Read (Key      => Org_Id,
                                    Is_Valid => Valid,
                                    Value    => Value);

      if not Valid then
         if not Is_Number (Org_Id) then
            raise GET_Parameter_Error with
              "org_id must be a valid natural integer";
         end if;

         Value := Get_Org_Contacts_Full_From_DB (Org_Id);
      end if;

      return TS (Value);
   end Get_Org_Contacts_Full;

   -------------------------------------
   --  Get_Org_Contacts_Full_From_DB  --
   -------------------------------------

   function Get_Org_Contacts_Full_From_DB
     (Org_Id : in String)
      return Unbounded_String
   is
      use Database;
      use Errors;

      Contacts : constant SQL_Left_Join_Table :=
                   Left_Join (Full    =>
                                Contactentity,
                              Partial =>
                                Organization_Contactentities,
                              On      =>
                                Contactentity.Ce_Id =
                                  Organization_Contactentities.Ce_Id);

      CA_Join : constant SQL_Left_Join_Table :=
                   Left_Join (Full    =>
                                Contacts,
                              Partial =>
                                Contactentity_Attributes,
                              On      =>
                                Contactentity.Ce_Id =
                                  Contactentity_Attributes.Ce_Id);

      Query    : constant SQL_Query :=
                   SQL_Select (Fields =>
                                 Contactentity.Json &
                                 Contactentity.Ce_Id &
                                 Contactentity.Ce_Name &
                                 Contactentity.Is_Human &
                                 Contactentity_Attributes.Json &
                                 Contactentity_Attributes.Org_Id &
                                 Contactentity_Attributes.Ce_Id,
                               From   => CA_Join,
                               Where  =>
                                 Organization_Contactentities.Org_Id =
                                   Natural'Value (Org_Id) and
                                 Contactentity_Attributes.Org_Id =
                                   Natural'Value (Org_Id));

      Cursor         : Exec.Forward_Cursor;
      DB_Connections : Database_Connection_Pool := Get_DB_Connections;
      Value          : Unbounded_String         := Null_Unbounded_String;
   begin
      Fetch_Data :
      for k in DB_Connections'Range loop
         Cursor.Fetch (DB_Connections (k).Host, Query);

         if DB_Connections (k).Host.Success then
            JSONIFY.Org_Contacts_Full (Cursor, Value);

            if Cursor.Processed_Rows > 0 then
               Org_Contacts_Full_Cache.Write (Key   => Org_Id,
                                              Value => Value);
            end if;

            exit Fetch_Data;
         else
            Failed_Query (Connection_Pool => DB_Connections,
                          Connection_Type => k);
         end if;
      end loop Fetch_Data;

      return Value;
   end Get_Org_Contacts_Full_From_DB;

   ------------------------
   --  Get_Organization  --
   ------------------------

   function Get_Organization
     (Org_Id : in String)
      return String
   is
      use AWS.Utils;
      use Errors;
      use Yolk.Utilities;

      Valid : Boolean          := False;
      Value : Unbounded_String := Null_Unbounded_String;
   begin
      Organization_Cache.Read (Key      => Org_Id,
                               Is_Valid => Valid,
                               Value    => Value);

      if not Valid then
         if not Is_Number (Org_Id) then
            raise GET_Parameter_Error with
              "org_id must be a valid Natural integer";
         end if;

         Value := Get_Organization_From_DB (Org_Id);
      end if;

      return TS (Value);
   end Get_Organization;

   --------------------------------
   --  Get_Organization_From_DB  --
   --------------------------------

   function Get_Organization_From_DB
     (Org_Id : in String)
      return Unbounded_String
   is
      use Database;
      use Errors;

      Query : constant SQL_Query :=
                SQL_Select (Fields        =>
                              Organization.Json &
                              Organization.Org_Id &
                              Organization.Org_Name &
                              Organization.Identifier,
                            Where         =>
                              Organization.Org_Id = Natural'Value (Org_Id),
                            Auto_Complete => True);

      Cursor         : Exec.Forward_Cursor;
      DB_Connections : Database_Connection_Pool := Get_DB_Connections;
      Value          : Unbounded_String         := Null_Unbounded_String;
   begin
      Fetch_Data :
      for k in DB_Connections'Range loop
         Cursor.Fetch (DB_Connections (k).Host, Query);

         if DB_Connections (k).Host.Success then
            JSONIFY.Organization (Cursor, Value);

            if Cursor.Processed_Rows > 0 then
               Organization_Cache.Write (Key   => Org_Id,
                                         Value => Value);
            end if;

            exit Fetch_Data;
         else
            Failed_Query (Connection_Pool => DB_Connections,
                          Connection_Type => k);
         end if;
      end loop Fetch_Data;

      return Value;
   end Get_Organization_From_DB;

end Storage.Read;
