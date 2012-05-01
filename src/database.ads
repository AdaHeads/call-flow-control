with GNATCOLL.SQL; use GNATCOLL.SQL;
with Database_Names; use Database_Names;
package Database is
   pragma Style_Checks (Off);
   pragma Elaborate_Body;

   type T_Abstract_Attribute (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Attribute, Instance, Index) with
   record
      Attr_Description : SQL_Field_Text (Ta_Attribute, Instance, N_Attr_Description, Index);
      Attr_Id : SQL_Field_Integer (Ta_Attribute, Instance, N_Attr_Id, Index);
      Attr_Name : SQL_Field_Text (Ta_Attribute, Instance, N_Attr_Name, Index);
      Attr_Value : SQL_Field_Text (Ta_Attribute, Instance, N_Attr_Value, Index);
   end record;

   type T_Attribute (Instance : Cst_String_Access)
      is new T_Abstract_Attribute (Instance, -1) with null record;
   type T_Numbered_Attribute (Index : Integer)
      is new T_Abstract_Attribute (null, Index) with null record;

   type T_Abstract_Contactentity (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Contactentity, Instance, Index) with
   record
      Ce_Id : SQL_Field_Integer (Ta_Contactentity, Instance, N_Ce_Id, Index);
      Ce_Name : SQL_Field_Text (Ta_Contactentity, Instance, N_Ce_Name, Index);
      Employed_From : SQL_Field_Time (Ta_Contactentity, Instance, N_Employed_From, Index);
      Employed_To : SQL_Field_Time (Ta_Contactentity, Instance, N_Employed_To, Index);
      Isperson : SQL_Field_Boolean (Ta_Contactentity, Instance, N_Isperson, Index);
   end record;

   type T_Contactentity (Instance : Cst_String_Access)
      is new T_Abstract_Contactentity (Instance, -1) with null record;
   type T_Numbered_Contactentity (Index : Integer)
      is new T_Abstract_Contactentity (null, Index) with null record;

   type T_Abstract_Contactentity_Attribute (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Contactentity_Attribute, Instance, Index) with
   record
      Attr_Id : SQL_Field_Integer (Ta_Contactentity_Attribute, Instance, N_Attr_Id, Index);
      Ce_Id : SQL_Field_Integer (Ta_Contactentity_Attribute, Instance, N_Ce_Id, Index);
   end record;

   type T_Contactentity_Attribute (Instance : Cst_String_Access)
      is new T_Abstract_Contactentity_Attribute (Instance, -1) with null record;
   type T_Numbered_Contactentity_Attribute (Index : Integer)
      is new T_Abstract_Contactentity_Attribute (null, Index) with null record;

   type T_Abstract_Contactentity_Organization (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Contactentity_Organization, Instance, Index) with
   record
      Ce_Id : SQL_Field_Integer (Ta_Contactentity_Organization, Instance, N_Ce_Id, Index);
      Org_Id : SQL_Field_Integer (Ta_Contactentity_Organization, Instance, N_Org_Id, Index);
   end record;

   type T_Contactentity_Organization (Instance : Cst_String_Access)
      is new T_Abstract_Contactentity_Organization (Instance, -1) with null record;
   type T_Numbered_Contactentity_Organization (Index : Integer)
      is new T_Abstract_Contactentity_Organization (null, Index) with null record;

   type T_Abstract_Contactentity_Tag (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Contactentity_Tag, Instance, Index) with
   record
      Ce_Id : SQL_Field_Integer (Ta_Contactentity_Tag, Instance, N_Ce_Id, Index);
      Tag_Id : SQL_Field_Integer (Ta_Contactentity_Tag, Instance, N_Tag_Id, Index);
   end record;

   type T_Contactentity_Tag (Instance : Cst_String_Access)
      is new T_Abstract_Contactentity_Tag (Instance, -1) with null record;
   type T_Numbered_Contactentity_Tag (Index : Integer)
      is new T_Abstract_Contactentity_Tag (null, Index) with null record;

   type T_Abstract_Organization (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Organization, Instance, Index) with
   record
      Org_Id : SQL_Field_Integer (Ta_Organization, Instance, N_Org_Id, Index);
      Org_Name : SQL_Field_Text (Ta_Organization, Instance, N_Org_Name, Index);
      Sip_Uri : SQL_Field_Text (Ta_Organization, Instance, N_Sip_Uri, Index);
   end record;

   type T_Organization (Instance : Cst_String_Access)
      is new T_Abstract_Organization (Instance, -1) with null record;
   type T_Numbered_Organization (Index : Integer)
      is new T_Abstract_Organization (null, Index) with null record;

   type T_Abstract_Tag (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Tag, Instance, Index) with
   record
      Tag_Description : SQL_Field_Text (Ta_Tag, Instance, N_Tag_Description, Index);
      Tag_Id : SQL_Field_Integer (Ta_Tag, Instance, N_Tag_Id, Index);
      Tag_Name : SQL_Field_Text (Ta_Tag, Instance, N_Tag_Name, Index);
   end record;

   type T_Tag (Instance : Cst_String_Access)
      is new T_Abstract_Tag (Instance, -1) with null record;
   type T_Numbered_Tag (Index : Integer)
      is new T_Abstract_Tag (null, Index) with null record;

   function FK (Self : T_Contactentity_Attribute'Class; Foreign : T_Contactentity'Class) return SQL_Criteria;
   function FK (Self : T_Contactentity_Attribute'Class; Foreign : T_Attribute'Class) return SQL_Criteria;
   function FK (Self : T_Contactentity_Organization'Class; Foreign : T_Contactentity'Class) return SQL_Criteria;
   function FK (Self : T_Contactentity_Organization'Class; Foreign : T_Organization'Class) return SQL_Criteria;
   function FK (Self : T_Contactentity_Tag'Class; Foreign : T_Contactentity'Class) return SQL_Criteria;
   function FK (Self : T_Contactentity_Tag'Class; Foreign : T_Tag'Class) return SQL_Criteria;

   Attribute : T_Attribute (null);
   Contactentity : T_Contactentity (null);
   Contactentity_Attribute : T_Contactentity_Attribute (null);
   Contactentity_Organization : T_Contactentity_Organization (null);
   Contactentity_Tag : T_Contactentity_Tag (null);
   Organization : T_Organization (null);
   Tag : T_Tag (null);
end Database;
