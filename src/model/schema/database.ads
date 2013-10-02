with GNATCOLL.SQL; use GNATCOLL.SQL;
with Database_Names; use Database_Names;
package Database is
   pragma Style_Checks (Off);
   pragma Elaborate_Body;

   type T_Abstract_Address_Types (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Address_Types, Instance, Index) with
   record
      Value : SQL_Field_Text (Ta_Address_Types, Instance, N_Value, Index);
   end record;

   type T_Address_Types (Instance : Cst_String_Access)
      is new T_Abstract_Address_Types (Instance, -1) with null record;
   type T_Numbered_Address_Types (Index : Integer)
      is new T_Abstract_Address_Types (null, Index) with null record;

   type T_Abstract_Contact_Types (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Contact_Types, Instance, Index) with
   record
      Value : SQL_Field_Text (Ta_Contact_Types, Instance, N_Value, Index);
   end record;

   type T_Contact_Types (Instance : Cst_String_Access)
      is new T_Abstract_Contact_Types (Instance, -1) with null record;
   type T_Numbered_Contact_Types (Index : Integer)
      is new T_Abstract_Contact_Types (null, Index) with null record;

   type T_Abstract_Contacts (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Contacts, Instance, Index) with
   record
      Contact_Type : SQL_Field_Text (Ta_Contacts, Instance, N_Contact_Type, Index);
      Full_Name : SQL_Field_Text (Ta_Contacts, Instance, N_Full_Name, Index);
      ID : SQL_Field_Integer (Ta_Contacts, Instance, N_ID, Index);
   end record;

   type T_Contacts (Instance : Cst_String_Access)
      is new T_Abstract_Contacts (Instance, -1) with null record;
   type T_Numbered_Contacts (Index : Integer)
      is new T_Abstract_Contacts (null, Index) with null record;

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

   type T_Abstract_Distribution_Lists (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Distribution_Lists, Instance, Index) with
   record
      Owner_Contact_ID : SQL_Field_Integer (Ta_Distribution_Lists, Instance, N_Owner_Contact_ID, Index);
      Owner_Organization_ID : SQL_Field_Integer (Ta_Distribution_Lists, Instance, N_Owner_Organization_ID, Index);
      Recipient_Visibility : SQL_Field_Text (Ta_Distribution_Lists, Instance, N_Recipient_Visibility, Index);
      Send_To_Contact_ID : SQL_Field_Integer (Ta_Distribution_Lists, Instance, N_Send_To_Contact_ID, Index);
      Send_To_Organization_ID : SQL_Field_Integer (Ta_Distribution_Lists, Instance, N_Send_To_Organization_ID, Index);
   end record;

   type T_Distribution_Lists (Instance : Cst_String_Access)
      is new T_Abstract_Distribution_Lists (Instance, -1) with null record;
   type T_Numbered_Distribution_Lists (Index : Integer)
      is new T_Abstract_Distribution_Lists (null, Index) with null record;

   type T_Abstract_End_Points (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_End_Points, Instance, Index) with
   record
      Address : SQL_Field_Text (Ta_End_Points, Instance, N_Address, Index);
      Address_Type : SQL_Field_Text (Ta_End_Points, Instance, N_Address_Type, Index);
      Confidential : SQL_Field_Boolean (Ta_End_Points, Instance, N_Confidential, Index);
      Contact_ID : SQL_Field_Integer (Ta_End_Points, Instance, N_Contact_ID, Index);
      Messaging : SQL_Field_Boolean (Ta_End_Points, Instance, N_Messaging, Index);
      Organization_ID : SQL_Field_Integer (Ta_End_Points, Instance, N_Organization_ID, Index);
   end record;

   type T_End_Points (Instance : Cst_String_Access)
      is new T_Abstract_End_Points (Instance, -1) with null record;
   type T_Numbered_End_Points (Index : Integer)
      is new T_Abstract_End_Points (null, Index) with null record;

   type T_Abstract_Kinds (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Kinds, Instance, Index) with
   record
      Description : SQL_Field_Text (Ta_Kinds, Instance, N_Description, Index);
      ID : SQL_Field_Text (Ta_Kinds, Instance, N_ID, Index);
   end record;

   type T_Kinds (Instance : Cst_String_Access)
      is new T_Abstract_Kinds (Instance, -1) with null record;
   type T_Numbered_Kinds (Index : Integer)
      is new T_Abstract_Kinds (null, Index) with null record;

   type T_Abstract_Organization_Contacts (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Organization_Contacts, Instance, Index) with
   record
      Attributes : SQL_Field_JSON (Ta_Organization_Contacts, Instance, N_Attributes, Index);
      Contact_ID : SQL_Field_Integer (Ta_Organization_Contacts, Instance, N_Contact_ID, Index);
      Organization_ID : SQL_Field_Integer (Ta_Organization_Contacts, Instance, N_Organization_ID, Index);
      Wants_Messages : SQL_Field_Boolean (Ta_Organization_Contacts, Instance, N_Wants_Messages, Index);
   end record;

   type T_Organization_Contacts (Instance : Cst_String_Access)
      is new T_Abstract_Organization_Contacts (Instance, -1) with null record;
   type T_Numbered_Organization_Contacts (Index : Integer)
      is new T_Abstract_Organization_Contacts (null, Index) with null record;

   type T_Abstract_Organizations (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Organizations, Instance, Index) with
   record
      Full_Name : SQL_Field_Text (Ta_Organizations, Instance, N_Full_Name, Index);
      ID : SQL_Field_Integer (Ta_Organizations, Instance, N_ID, Index);
      JSON : SQL_Field_JSON (Ta_Organizations, Instance, N_JSON, Index);
      URI : SQL_Field_Text (Ta_Organizations, Instance, N_URI, Index);
   end record;

   type T_Organizations (Instance : Cst_String_Access)
      is new T_Abstract_Organizations (Instance, -1) with null record;
   type T_Numbered_Organizations (Index : Integer)
      is new T_Abstract_Organizations (null, Index) with null record;

   type T_Abstract_Recipient_Visibilities (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Recipient_Visibilities, Instance, Index) with
   record
      Value : SQL_Field_Text (Ta_Recipient_Visibilities, Instance, N_Value, Index);
   end record;

   type T_Recipient_Visibilities (Instance : Cst_String_Access)
      is new T_Abstract_Recipient_Visibilities (Instance, -1) with null record;
   type T_Numbered_Recipient_Visibilities (Index : Integer)
      is new T_Abstract_Recipient_Visibilities (null, Index) with null record;

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

   type T_Abstract_User_IDs (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_User_IDs, Instance, Index) with
   record
      Name : SQL_Field_Text (Ta_User_IDs, Instance, N_Name, Index);
      OpenID : SQL_Field_Text (Ta_User_IDs, Instance, N_OpenID, Index);
      Priority : SQL_Field_Integer (Ta_User_IDs, Instance, N_Priority, Index);
   end record;

   type T_User_IDs (Instance : Cst_String_Access)
      is new T_Abstract_User_IDs (Instance, -1) with null record;
   type T_Numbered_User_IDs (Index : Integer)
      is new T_Abstract_User_IDs (null, Index) with null record;

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

   function FK (Self : T_Contacts'Class; Foreign : T_Contact_Types'Class) return SQL_Criteria;
   function FK (Self : T_Distribution_Lists'Class; Foreign : T_Recipient_Visibilities'Class) return SQL_Criteria;
   function FK (Self : T_End_Points'Class; Foreign : T_Organization_Contacts'Class) return SQL_Criteria;
   function FK (Self : T_End_Points'Class; Foreign : T_Address_Types'Class) return SQL_Criteria;
   function FK (Self : T_Organization_Contacts'Class; Foreign : T_Organizations'Class) return SQL_Criteria;
   function FK (Self : T_Organization_Contacts'Class; Foreign : T_Contacts'Class) return SQL_Criteria;
   function FK (Self : T_Special_Days'Class; Foreign : T_Kinds'Class) return SQL_Criteria;
   function FK (Self : T_User_Ids'Class; Foreign : T_Users'Class) return SQL_Criteria;

   Address_Types : T_Address_Types (null);
   Contact_Types : T_Contact_Types (null);
   Contacts : T_Contacts (null);
   Dial_Plans : T_Dial_Plans (null);
   Distribution_Lists : T_Distribution_Lists (null);
   End_Points : T_End_Points (null);
   Kinds : T_Kinds (null);
   Organization_Contacts : T_Organization_Contacts (null);
   Organizations : T_Organizations (null);
   Recipient_Visibilities : T_Recipient_Visibilities (null);
   Special_Days : T_Special_Days (null);
   User_IDs : T_User_IDs (null);
   Users : T_Users (null);
end Database;
