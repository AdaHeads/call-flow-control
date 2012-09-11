with GNATCOLL.SQL; use GNATCOLL.SQL;
with Database_Names; use Database_Names;
package Database is
   pragma Style_Checks (Off);
   pragma Elaborate_Body;

   type T_Abstract_Contactentity (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Contactentity, Instance, Index) with
   record
      Ce_Id : SQL_Field_Integer (Ta_Contactentity, Instance, N_Ce_Id, Index);
      Ce_Name : SQL_Field_Text (Ta_Contactentity, Instance, N_Ce_Name, Index);
      Is_Human : SQL_Field_Boolean (Ta_Contactentity, Instance, N_Is_Human, Index);
   end record;

   type T_Contactentity (Instance : Cst_String_Access)
      is new T_Abstract_Contactentity (Instance, -1) with null record;
   type T_Numbered_Contactentity (Index : Integer)
      is new T_Abstract_Contactentity (null, Index) with null record;

   type T_Abstract_Contactentity_Attributes (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Contactentity_Attributes, Instance, Index) with
   record
      Ce_Id : SQL_Field_Integer (Ta_Contactentity_Attributes, Instance, N_Ce_Id, Index);
      Json : SQL_Field_Text (Ta_Contactentity_Attributes, Instance, N_Json, Index);
      Org_Id : SQL_Field_Integer (Ta_Contactentity_Attributes, Instance, N_Org_Id, Index);
   end record;

   type T_Contactentity_Attributes (Instance : Cst_String_Access)
      is new T_Abstract_Contactentity_Attributes (Instance, -1) with null record;
   type T_Numbered_Contactentity_Attributes (Index : Integer)
      is new T_Abstract_Contactentity_Attributes (null, Index) with null record;

   type T_Abstract_Organization (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Organization, Instance, Index) with
   record
      Identifier : SQL_Field_Text (Ta_Organization, Instance, N_Identifier, Index);
      Json : SQL_Field_Text (Ta_Organization, Instance, N_Json, Index);
      Org_Id : SQL_Field_Integer (Ta_Organization, Instance, N_Org_Id, Index);
      Org_Name : SQL_Field_Text (Ta_Organization, Instance, N_Org_Name, Index);
   end record;

   type T_Organization (Instance : Cst_String_Access)
      is new T_Abstract_Organization (Instance, -1) with null record;
   type T_Numbered_Organization (Index : Integer)
      is new T_Abstract_Organization (null, Index) with null record;

   type T_Abstract_Organization_Contactentities (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Organization_Contactentities, Instance, Index) with
   record
      Ce_Id : SQL_Field_Integer (Ta_Organization_Contactentities, Instance, N_Ce_Id, Index);
      Org_Id : SQL_Field_Integer (Ta_Organization_Contactentities, Instance, N_Org_Id, Index);
   end record;

   type T_Organization_Contactentities (Instance : Cst_String_Access)
      is new T_Abstract_Organization_Contactentities (Instance, -1) with null record;
   type T_Numbered_Organization_Contactentities (Index : Integer)
      is new T_Abstract_Organization_Contactentities (null, Index) with null record;

   function FK (Self : T_Contactentity_Attributes'Class; Foreign : T_Organization'Class) return SQL_Criteria;
   function FK (Self : T_Contactentity_Attributes'Class; Foreign : T_Contactentity'Class) return SQL_Criteria;
   function FK (Self : T_Contactentity_Attributes'Class; Foreign : T_Organization_Contactentities'Class) return SQL_Criteria;
   function FK (Self : T_Organization_Contactentities'Class; Foreign : T_Organization'Class) return SQL_Criteria;
   function FK (Self : T_Organization_Contactentities'Class; Foreign : T_Contactentity'Class) return SQL_Criteria;

   Contactentity : T_Contactentity (null);
   Contactentity_Attributes : T_Contactentity_Attributes (null);
   Organization : T_Organization (null);
   Organization_Contactentities : T_Organization_Contactentities (null);
end Database;
