with GNATCOLL.SQL; use GNATCOLL.SQL;
with Database_Names; use Database_Names;
package Database is
   pragma Style_Checks (Off);
   pragma Elaborate_Body;

   subtype Kind_Id is Integer;
   Kind_Bcc : constant Kind_Id := 3;
   Kind_Cc : constant Kind_Id := 2;
   Kind_To : constant Kind_Id := 1;

   type T_Abstract_Contactentity (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Contactentity, Instance, Index) with
   record
      Full_Name : SQL_Field_Text (Ta_Contactentity, Instance, N_Full_Name, Index);
      Id : SQL_Field_Integer (Ta_Contactentity, Instance, N_Id, Index);
      Is_Human : SQL_Field_Boolean (Ta_Contactentity, Instance, N_Is_Human, Index);
   end record;

   type T_Contactentity (Instance : Cst_String_Access)
      is new T_Abstract_Contactentity (Instance, -1) with null record;
   type T_Numbered_Contactentity (Index : Integer)
      is new T_Abstract_Contactentity (null, Index) with null record;

   type T_Abstract_Contactentity_Attributes (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Contactentity_Attributes, Instance, Index) with
   record
      Contactentity_Id : SQL_Field_Integer (Ta_Contactentity_Attributes, Instance, N_Contactentity_Id, Index);
      Json : SQL_Field_Json (Ta_Contactentity_Attributes, Instance, N_Json, Index);
      Organization_Id : SQL_Field_Integer (Ta_Contactentity_Attributes, Instance, N_Organization_Id, Index);
   end record;

   type T_Contactentity_Attributes (Instance : Cst_String_Access)
      is new T_Abstract_Contactentity_Attributes (Instance, -1) with null record;
   type T_Numbered_Contactentity_Attributes (Index : Integer)
      is new T_Abstract_Contactentity_Attributes (null, Index) with null record;

   type T_Abstract_Contactentity_Recipient (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Contactentity_Recipient, Instance, Index) with
   record
      Contactentity_Id : SQL_Field_Integer (Ta_Contactentity_Recipient, Instance, N_Contactentity_Id, Index);
      Recipient_Id : SQL_Field_Integer (Ta_Contactentity_Recipient, Instance, N_Recipient_Id, Index);
   end record;

   type T_Contactentity_Recipient (Instance : Cst_String_Access)
      is new T_Abstract_Contactentity_Recipient (Instance, -1) with null record;
   type T_Numbered_Contactentity_Recipient (Index : Integer)
      is new T_Abstract_Contactentity_Recipient (null, Index) with null record;

   type T_Abstract_Organization (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Organization, Instance, Index) with
   record
      Id : SQL_Field_Integer (Ta_Organization, Instance, N_Id, Index);
      Identifier : SQL_Field_Text (Ta_Organization, Instance, N_Identifier, Index);
      Json : SQL_Field_Json (Ta_Organization, Instance, N_Json, Index);
      Name : SQL_Field_Text (Ta_Organization, Instance, N_Name, Index);
   end record;

   type T_Organization (Instance : Cst_String_Access)
      is new T_Abstract_Organization (Instance, -1) with null record;
   type T_Numbered_Organization (Index : Integer)
      is new T_Abstract_Organization (null, Index) with null record;

   type T_Abstract_Organization_Contactentities (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Organization_Contactentities, Instance, Index) with
   record
      Contactentity_Id : SQL_Field_Integer (Ta_Organization_Contactentities, Instance, N_Contactentity_Id, Index);
      Organization_Id : SQL_Field_Integer (Ta_Organization_Contactentities, Instance, N_Organization_Id, Index);
   end record;

   type T_Organization_Contactentities (Instance : Cst_String_Access)
      is new T_Abstract_Organization_Contactentities (Instance, -1) with null record;
   type T_Numbered_Organization_Contactentities (Index : Integer)
      is new T_Abstract_Organization_Contactentities (null, Index) with null record;

   type T_Abstract_Recipient (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Recipient, Instance, Index) with
   record
      Email_Address : SQL_Field_Text (Ta_Recipient, Instance, N_Email_Address, Index);
      Full_Name : SQL_Field_Text (Ta_Recipient, Instance, N_Full_Name, Index);
      Id : SQL_Field_Integer (Ta_Recipient, Instance, N_Id, Index);
      Kind_Id : SQL_Field_Integer (Ta_Recipient, Instance, N_Kind_Id, Index);
   end record;

   type T_Recipient (Instance : Cst_String_Access)
      is new T_Abstract_Recipient (Instance, -1) with null record;
   type T_Numbered_Recipient (Index : Integer)
      is new T_Abstract_Recipient (null, Index) with null record;

   type T_Abstract_Recipient_Kind (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Recipient_Kind, Instance, Index) with
   record
      Id : SQL_Field_Integer (Ta_Recipient_Kind, Instance, N_Id, Index);
      Kind : SQL_Field_Text (Ta_Recipient_Kind, Instance, N_Kind, Index);
   end record;

   type T_Recipient_Kind (Instance : Cst_String_Access)
      is new T_Abstract_Recipient_Kind (Instance, -1) with null record;
   type T_Numbered_Recipient_Kind (Index : Integer)
      is new T_Abstract_Recipient_Kind (null, Index) with null record;

   function FK (Self : T_Contactentity_Attributes'Class; Foreign : T_Organization'Class) return SQL_Criteria;
   function FK (Self : T_Contactentity_Attributes'Class; Foreign : T_Organization_Contactentities'Class) return SQL_Criteria;
   function FK (Self : T_Contactentity_Attributes'Class; Foreign : T_Contactentity'Class) return SQL_Criteria;
   function FK (Self : T_Contactentity_Recipient'Class; Foreign : T_Contactentity'Class) return SQL_Criteria;
   function FK (Self : T_Contactentity_Recipient'Class; Foreign : T_Recipient'Class) return SQL_Criteria;
   function FK (Self : T_Organization_Contactentities'Class; Foreign : T_Organization'Class) return SQL_Criteria;
   function FK (Self : T_Organization_Contactentities'Class; Foreign : T_Contactentity'Class) return SQL_Criteria;
   function FK (Self : T_Recipient'Class; Foreign : T_Recipient_Kind'Class) return SQL_Criteria;

   Contactentity : T_Contactentity (null);
   Contactentity_Attributes : T_Contactentity_Attributes (null);
   Contactentity_Recipient : T_Contactentity_Recipient (null);
   Organization : T_Organization (null);
   Organization_Contactentities : T_Organization_Contactentities (null);
   Recipient : T_Recipient (null);
   Recipient_Kind : T_Recipient_Kind (null);
end Database;
