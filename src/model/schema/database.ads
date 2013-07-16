with GNATCOLL.SQL; use GNATCOLL.SQL;
with Database_Names; use Database_Names;
package Database is
   pragma Style_Checks (Off);
   pragma Elaborate_Body;

   subtype Kind_Id is Integer;
   Kind_Bcc : constant Kind_Id := 3;
   Kind_Cc : constant Kind_Id := 2;
   Kind_To : constant Kind_Id := 1;

   type T_Abstract_Dial_Plans (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Dial_Plans, Instance, Index) with
   record
      Dial_Plan : SQL_Field_XML (Ta_Dial_Plans, Instance, N_Dial_Plan, Index);
      Phone_Number : SQL_Field_Text (Ta_Dial_Plans, Instance, N_Phone_Number, Index);
   end record;

   type T_Dial_Plans (Instance : Cst_String_Access)
      is new T_Abstract_Dial_Plans (Instance, -1) with null record;
   type T_Numbered_Dial_Plans (Index : Integer)
      is new T_Abstract_Dial_Plans (null, Index) with null record;

   type T_Abstract_Contact (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Contact, Instance, Index) with
   record
      Full_Name : SQL_Field_Text (Ta_Contact, Instance, N_Full_Name, Index);
      Id : SQL_Field_Integer (Ta_Contact, Instance, N_Id, Index);
      Is_Human : SQL_Field_Boolean (Ta_Contact, Instance, N_Is_Human, Index);
   end record;

   type T_Contact (Instance : Cst_String_Access)
      is new T_Abstract_Contact (Instance, -1) with null record;
   type T_Numbered_Contact (Index : Integer)
      is new T_Abstract_Contact (null, Index) with null record;

   type T_Abstract_Contact_Attributes (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Contact_Attributes, Instance, Index) with
   record
      Contact_Id : SQL_Field_Integer (Ta_Contact_Attributes, Instance, N_Contact_Id, Index);
      Json : SQL_Field_Json (Ta_Contact_Attributes, Instance, N_Json, Index);
      Organization_Id : SQL_Field_Integer (Ta_Contact_Attributes, Instance, N_Organization_Id, Index);
   end record;

   type T_Contact_Attributes (Instance : Cst_String_Access)
      is new T_Abstract_Contact_Attributes (Instance, -1) with null record;
   type T_Numbered_Contact_Attributes (Index : Integer)
      is new T_Abstract_Contact_Attributes (null, Index) with null record;

   type T_Abstract_Contact_Recipients (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Contact_Recipients, Instance, Index) with
   record
      Contact_Id : SQL_Field_Integer (Ta_Contact_Recipients, Instance, N_Contact_Id, Index);
      Recipient_Id : SQL_Field_Integer (Ta_Contact_Recipients, Instance, N_Recipient_Id, Index);
   end record;

   type T_Contact_Recipients (Instance : Cst_String_Access)
      is new T_Abstract_Contact_Recipients (Instance, -1) with null record;
   type T_Numbered_Contact_Recipients (Index : Integer)
      is new T_Abstract_Contact_Recipients (null, Index) with null record;

   type T_Abstract_Kinds (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Kinds, Instance, Index) with
   record
      Description : SQL_Field_Text (Ta_Kinds, Instance, N_Description, Index);
      Id : SQL_Field_Text (Ta_Kinds, Instance, N_Id, Index);
   end record;

   type T_Kinds (Instance : Cst_String_Access)
      is new T_Abstract_Kinds (Instance, -1) with null record;
   type T_Numbered_Kinds (Index : Integer)
      is new T_Abstract_Kinds (null, Index) with null record;

   type T_Abstract_Organization (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Organization, Instance, Index) with
   record
      Full_Name : SQL_Field_Text (Ta_Organization, Instance, N_Full_Name, Index);
      Id : SQL_Field_Integer (Ta_Organization, Instance, N_Id, Index);
      Json : SQL_Field_Json (Ta_Organization, Instance, N_Json, Index);
      Uri : SQL_Field_Text (Ta_Organization, Instance, N_Uri, Index);
   end record;

   type T_Organization (Instance : Cst_String_Access)
      is new T_Abstract_Organization (Instance, -1) with null record;
   type T_Numbered_Organization (Index : Integer)
      is new T_Abstract_Organization (null, Index) with null record;

   type T_Abstract_Organization_Contacts (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Organization_Contacts, Instance, Index) with
   record
      Contact_Id : SQL_Field_Integer (Ta_Organization_Contacts, Instance, N_Contact_Id, Index);
      Organization_Id : SQL_Field_Integer (Ta_Organization_Contacts, Instance, N_Organization_Id, Index);
   end record;

   type T_Organization_Contacts (Instance : Cst_String_Access)
      is new T_Abstract_Organization_Contacts (Instance, -1) with null record;
   type T_Numbered_Organization_Contacts (Index : Integer)
      is new T_Abstract_Organization_Contacts (null, Index) with null record;

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

   type T_Abstract_Special_Days (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Special_Days, Instance, Index) with
   record
      Day : SQL_Field_Date (Ta_Special_Days, Instance, N_Day, Index);
      Kind : SQL_Field_Text (Ta_Special_Days, Instance, N_Kind, Index);
   end record;

   type T_Special_Days (Instance : Cst_String_Access)
      is new T_Abstract_Special_Days (Instance, -1) with null record;
   type T_Numbered_Special_Days (Index : Integer)
      is new T_Abstract_Special_Days (null, Index) with null record;

   type T_Abstract_User_Ids (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_User_Ids, Instance, Index) with
   record
      Name : SQL_Field_Text (Ta_User_Ids, Instance, N_Name, Index);
      Openid : SQL_Field_Text (Ta_User_Ids, Instance, N_Openid, Index);
      Rank : SQL_Field_Integer (Ta_User_Ids, Instance, N_Rank, Index);
   end record;

   type T_User_Ids (Instance : Cst_String_Access)
      is new T_Abstract_User_Ids (Instance, -1) with null record;
   type T_Numbered_User_Ids (Index : Integer)
      is new T_Abstract_User_Ids (null, Index) with null record;

   type T_Abstract_Users (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Users, Instance, Index) with
   record
      Is_Administrator : SQL_Field_Boolean (Ta_Users, Instance, N_Is_Administrator, Index);
      Is_Receptionist : SQL_Field_Boolean (Ta_Users, Instance, N_Is_Receptionist, Index);
      Is_Service_Agent : SQL_Field_Boolean (Ta_Users, Instance, N_Is_Service_Agent, Index);
      Name : SQL_Field_Text (Ta_Users, Instance, N_Name, Index);
   end record;

   type T_Users (Instance : Cst_String_Access)
      is new T_Abstract_Users (Instance, -1) with null record;
   type T_Numbered_Users (Index : Integer)
      is new T_Abstract_Users (null, Index) with null record;

   function FK (Self : T_Contact_Attributes'Class; Foreign : T_Organization'Class) return SQL_Criteria;
   function FK (Self : T_Contact_Attributes'Class; Foreign : T_Organization_Contacts'Class) return SQL_Criteria;
   function FK (Self : T_Contact_Attributes'Class; Foreign : T_Contact'Class) return SQL_Criteria;
   function FK (Self : T_Contact_Recipients'Class; Foreign : T_Contact'Class) return SQL_Criteria;
   function FK (Self : T_Contact_Recipients'Class; Foreign : T_Recipient'Class) return SQL_Criteria;
   function FK (Self : T_Organization_Contacts'Class; Foreign : T_Organization'Class) return SQL_Criteria;
   function FK (Self : T_Organization_Contacts'Class; Foreign : T_Contact'Class) return SQL_Criteria;
   function FK (Self : T_Recipient'Class; Foreign : T_Recipient_Kind'Class) return SQL_Criteria;
   function FK (Self : T_Special_Days'Class; Foreign : T_Kinds'Class) return SQL_Criteria;
   function FK (Self : T_User_Ids'Class; Foreign : T_Users'Class) return SQL_Criteria;

   Dial_Plans : T_Dial_Plans (null);
   Contact : T_Contact (null);
   Contact_Attributes : T_Contact_Attributes (null);
   Contact_Recipients : T_Contact_Recipients (null);
   Kinds : T_Kinds (null);
   Organization : T_Organization (null);
   Organization_Contacts : T_Organization_Contacts (null);
   Recipient : T_Recipient (null);
   Recipient_Kind : T_Recipient_Kind (null);
   Special_Days : T_Special_Days (null);
   User_Ids : T_User_Ids (null);
   Users : T_Users (null);
end Database;
