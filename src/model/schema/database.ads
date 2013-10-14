with GNATCOLL.SQL; use GNATCOLL.SQL;
with Database_Names; use Database_Names;
package Database is
   pragma Style_Checks (Off);
   pragma Elaborate_Body;

   type T_Abstract_Archive_Message_Queue (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Archive_Message_Queue, Instance, Index) with
   record
      Created_At : SQL_Field_Time (Ta_Archive_Message_Queue, Instance, N_Created_At, Index);
      ID : SQL_Field_Integer (Ta_Archive_Message_Queue, Instance, N_ID, Index);
      Last_Try : SQL_Field_Time (Ta_Archive_Message_Queue, Instance, N_Last_Try, Index);
      Message : SQL_Field_Text (Ta_Archive_Message_Queue, Instance, N_Message, Index);
      Subject : SQL_Field_Text (Ta_Archive_Message_Queue, Instance, N_Subject, Index);
      Taken_By_Agent : SQL_Field_Text (Ta_Archive_Message_Queue, Instance, N_Taken_By_Agent, Index);
      Taken_From : SQL_Field_Text (Ta_Archive_Message_Queue, Instance, N_Taken_From, Index);
      To_Contact_ID : SQL_Field_Integer (Ta_Archive_Message_Queue, Instance, N_To_Contact_ID, Index);
      Tries : SQL_Field_Integer (Ta_Archive_Message_Queue, Instance, N_Tries, Index);
      Urgent : SQL_Field_Boolean (Ta_Archive_Message_Queue, Instance, N_Urgent, Index);
   end record;

   type T_Archive_Message_Queue (Instance : Cst_String_Access)
      is new T_Abstract_Archive_Message_Queue (Instance, -1) with null record;
   type T_Numbered_Archive_Message_Queue (Index : Integer)
      is new T_Abstract_Archive_Message_Queue (null, Index) with null record;

   type T_Abstract_Archive_Message_Queue_Recipients (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Archive_Message_Queue_Recipients, Instance, Index) with
   record
      Contact_ID : SQL_Field_Integer (Ta_Archive_Message_Queue_Recipients, Instance, N_Contact_ID, Index);
      Message_ID : SQL_Field_Integer (Ta_Archive_Message_Queue_Recipients, Instance, N_Message_ID, Index);
      Organization_ID : SQL_Field_Integer (Ta_Archive_Message_Queue_Recipients, Instance, N_Organization_ID, Index);
      Recipient_Role : SQL_Field_Text (Ta_Archive_Message_Queue_Recipients, Instance, N_Recipient_Role, Index);
      Resolved_Addresses : SQL_Field_Text (Ta_Archive_Message_Queue_Recipients, Instance, N_Resolved_Addresses, Index);
   end record;

   type T_Archive_Message_Queue_Recipients (Instance : Cst_String_Access)
      is new T_Abstract_Archive_Message_Queue_Recipients (Instance, -1) with null record;
   type T_Numbered_Archive_Message_Queue_Recipients (Index : Integer)
      is new T_Abstract_Archive_Message_Queue_Recipients (null, Index) with null record;

   type T_Abstract_Calendar_Events (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Calendar_Events, Instance, Index) with
   record
      ID : SQL_Field_Integer (Ta_Calendar_Events, Instance, N_ID, Index);
      Message : SQL_Field_Text (Ta_Calendar_Events, Instance, N_Message, Index);
      Start : SQL_Field_Time (Ta_Calendar_Events, Instance, N_Start, Index);
      Stop : SQL_Field_Time (Ta_Calendar_Events, Instance, N_Stop, Index);
   end record;

   type T_Calendar_Events (Instance : Cst_String_Access)
      is new T_Abstract_Calendar_Events (Instance, -1) with null record;
   type T_Numbered_Calendar_Events (Index : Integer)
      is new T_Abstract_Calendar_Events (null, Index) with null record;

   type T_Abstract_Contact_Calendar (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Contact_Calendar, Instance, Index) with
   record
      Contact_ID : SQL_Field_Integer (Ta_Contact_Calendar, Instance, N_Contact_ID, Index);
      Event_ID : SQL_Field_Integer (Ta_Contact_Calendar, Instance, N_Event_ID, Index);
      Organization_ID : SQL_Field_Integer (Ta_Contact_Calendar, Instance, N_Organization_ID, Index);
   end record;

   type T_Contact_Calendar (Instance : Cst_String_Access)
      is new T_Abstract_Contact_Calendar (Instance, -1) with null record;
   type T_Numbered_Contact_Calendar (Index : Integer)
      is new T_Abstract_Contact_Calendar (null, Index) with null record;

   type T_Abstract_Contact_Phone_Numbers (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Contact_Phone_Numbers, Instance, Index) with
   record
      Contact_ID : SQL_Field_Integer (Ta_Contact_Phone_Numbers, Instance, N_Contact_ID, Index);
      Organization_ID : SQL_Field_Integer (Ta_Contact_Phone_Numbers, Instance, N_Organization_ID, Index);
      Phone_Number_ID : SQL_Field_Integer (Ta_Contact_Phone_Numbers, Instance, N_Phone_Number_ID, Index);
   end record;

   type T_Contact_Phone_Numbers (Instance : Cst_String_Access)
      is new T_Abstract_Contact_Phone_Numbers (Instance, -1) with null record;
   type T_Numbered_Contact_Phone_Numbers (Index : Integer)
      is new T_Abstract_Contact_Phone_Numbers (null, Index) with null record;

   type T_Abstract_Contact_Recurring_Calendar (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Contact_Recurring_Calendar, Instance, Index) with
   record
      Contact_ID : SQL_Field_Integer (Ta_Contact_Recurring_Calendar, Instance, N_Contact_ID, Index);
      Event_ID : SQL_Field_Integer (Ta_Contact_Recurring_Calendar, Instance, N_Event_ID, Index);
      Organization_ID : SQL_Field_Integer (Ta_Contact_Recurring_Calendar, Instance, N_Organization_ID, Index);
   end record;

   type T_Contact_Recurring_Calendar (Instance : Cst_String_Access)
      is new T_Abstract_Contact_Recurring_Calendar (Instance, -1) with null record;
   type T_Numbered_Contact_Recurring_Calendar (Index : Integer)
      is new T_Abstract_Contact_Recurring_Calendar (null, Index) with null record;

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
      Enabled : SQL_Field_Boolean (Ta_Contacts, Instance, N_Enabled, Index);
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
      ID : SQL_Field_Integer (Ta_Distribution_Lists, Instance, N_ID, Index);
      Recipient_Visibility : SQL_Field_Text (Ta_Distribution_Lists, Instance, N_Recipient_Visibility, Index);
      Send_To_Contact_ID : SQL_Field_Integer (Ta_Distribution_Lists, Instance, N_Send_To_Contact_ID, Index);
      Send_To_Organization_ID : SQL_Field_Integer (Ta_Distribution_Lists, Instance, N_Send_To_Organization_ID, Index);
   end record;

   type T_Distribution_Lists (Instance : Cst_String_Access)
      is new T_Abstract_Distribution_Lists (Instance, -1) with null record;
   type T_Numbered_Distribution_Lists (Index : Integer)
      is new T_Abstract_Distribution_Lists (null, Index) with null record;

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

   type T_Abstract_Message_Queue (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Message_Queue, Instance, Index) with
   record
      Created_At : SQL_Field_Time (Ta_Message_Queue, Instance, N_Created_At, Index);
      ID : SQL_Field_Integer (Ta_Message_Queue, Instance, N_ID, Index);
      Last_Try : SQL_Field_Time (Ta_Message_Queue, Instance, N_Last_Try, Index);
      Message : SQL_Field_Text (Ta_Message_Queue, Instance, N_Message, Index);
      Subject : SQL_Field_Text (Ta_Message_Queue, Instance, N_Subject, Index);
      Taken_By_Agent : SQL_Field_Text (Ta_Message_Queue, Instance, N_Taken_By_Agent, Index);
      Taken_From : SQL_Field_Text (Ta_Message_Queue, Instance, N_Taken_From, Index);
      To_Contact_ID : SQL_Field_Integer (Ta_Message_Queue, Instance, N_To_Contact_ID, Index);
      Tries : SQL_Field_Integer (Ta_Message_Queue, Instance, N_Tries, Index);
      Urgent : SQL_Field_Boolean (Ta_Message_Queue, Instance, N_Urgent, Index);
   end record;

   type T_Message_Queue (Instance : Cst_String_Access)
      is new T_Abstract_Message_Queue (Instance, -1) with null record;
   type T_Numbered_Message_Queue (Index : Integer)
      is new T_Abstract_Message_Queue (null, Index) with null record;

   type T_Abstract_Message_Queue_Recipients (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Message_Queue_Recipients, Instance, Index) with
   record
      Contact_ID : SQL_Field_Integer (Ta_Message_Queue_Recipients, Instance, N_Contact_ID, Index);
      Message_ID : SQL_Field_Integer (Ta_Message_Queue_Recipients, Instance, N_Message_ID, Index);
      Organization_ID : SQL_Field_Integer (Ta_Message_Queue_Recipients, Instance, N_Organization_ID, Index);
      Recipient_Role : SQL_Field_Text (Ta_Message_Queue_Recipients, Instance, N_Recipient_Role, Index);
   end record;

   type T_Message_Queue_Recipients (Instance : Cst_String_Access)
      is new T_Abstract_Message_Queue_Recipients (Instance, -1) with null record;
   type T_Numbered_Message_Queue_Recipients (Index : Integer)
      is new T_Abstract_Message_Queue_Recipients (null, Index) with null record;

   type T_Abstract_Messaging_Address_Types (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Messaging_Address_Types, Instance, Index) with
   record
      Value : SQL_Field_Text (Ta_Messaging_Address_Types, Instance, N_Value, Index);
   end record;

   type T_Messaging_Address_Types (Instance : Cst_String_Access)
      is new T_Abstract_Messaging_Address_Types (Instance, -1) with null record;
   type T_Numbered_Messaging_Address_Types (Index : Integer)
      is new T_Abstract_Messaging_Address_Types (null, Index) with null record;

   type T_Abstract_Messaging_Addresses (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Messaging_Addresses, Instance, Index) with
   record
      Address : SQL_Field_Text (Ta_Messaging_Addresses, Instance, N_Address, Index);
      Address_Type : SQL_Field_Text (Ta_Messaging_Addresses, Instance, N_Address_Type, Index);
      ID : SQL_Field_Integer (Ta_Messaging_Addresses, Instance, N_ID, Index);
   end record;

   type T_Messaging_Addresses (Instance : Cst_String_Access)
      is new T_Abstract_Messaging_Addresses (Instance, -1) with null record;
   type T_Numbered_Messaging_Addresses (Index : Integer)
      is new T_Abstract_Messaging_Addresses (null, Index) with null record;

   type T_Abstract_Messaging_End_Points (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Messaging_End_Points, Instance, Index) with
   record
      Address_ID : SQL_Field_Integer (Ta_Messaging_End_Points, Instance, N_Address_ID, Index);
      Confidential : SQL_Field_Boolean (Ta_Messaging_End_Points, Instance, N_Confidential, Index);
      Contact_ID : SQL_Field_Integer (Ta_Messaging_End_Points, Instance, N_Contact_ID, Index);
      Enabled : SQL_Field_Boolean (Ta_Messaging_End_Points, Instance, N_Enabled, Index);
      Organization_ID : SQL_Field_Integer (Ta_Messaging_End_Points, Instance, N_Organization_ID, Index);
      Priority : SQL_Field_Integer (Ta_Messaging_End_Points, Instance, N_Priority, Index);
   end record;

   type T_Messaging_End_Points (Instance : Cst_String_Access)
      is new T_Abstract_Messaging_End_Points (Instance, -1) with null record;
   type T_Numbered_Messaging_End_Points (Index : Integer)
      is new T_Abstract_Messaging_End_Points (null, Index) with null record;

   type T_Abstract_Organization_Calendar (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Organization_Calendar, Instance, Index) with
   record
      Event_ID : SQL_Field_Integer (Ta_Organization_Calendar, Instance, N_Event_ID, Index);
      Organization_ID : SQL_Field_Integer (Ta_Organization_Calendar, Instance, N_Organization_ID, Index);
   end record;

   type T_Organization_Calendar (Instance : Cst_String_Access)
      is new T_Abstract_Organization_Calendar (Instance, -1) with null record;
   type T_Numbered_Organization_Calendar (Index : Integer)
      is new T_Abstract_Organization_Calendar (null, Index) with null record;

   type T_Abstract_Organization_Contacts (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Organization_Contacts, Instance, Index) with
   record
      Attributes : SQL_Field_JSON (Ta_Organization_Contacts, Instance, N_Attributes, Index);
      Contact_ID : SQL_Field_Integer (Ta_Organization_Contacts, Instance, N_Contact_ID, Index);
      Distribution_List_ID : SQL_Field_Integer (Ta_Organization_Contacts, Instance, N_Distribution_List_ID, Index);
      Enabled : SQL_Field_Boolean (Ta_Organization_Contacts, Instance, N_Enabled, Index);
      Organization_ID : SQL_Field_Integer (Ta_Organization_Contacts, Instance, N_Organization_ID, Index);
      Wants_Messages : SQL_Field_Boolean (Ta_Organization_Contacts, Instance, N_Wants_Messages, Index);
   end record;

   type T_Organization_Contacts (Instance : Cst_String_Access)
      is new T_Abstract_Organization_Contacts (Instance, -1) with null record;
   type T_Numbered_Organization_Contacts (Index : Integer)
      is new T_Abstract_Organization_Contacts (null, Index) with null record;

   type T_Abstract_Organization_Recurring_Calendar (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Organization_Recurring_Calendar, Instance, Index) with
   record
      Event_ID : SQL_Field_Integer (Ta_Organization_Recurring_Calendar, Instance, N_Event_ID, Index);
      Organization_ID : SQL_Field_Integer (Ta_Organization_Recurring_Calendar, Instance, N_Organization_ID, Index);
   end record;

   type T_Organization_Recurring_Calendar (Instance : Cst_String_Access)
      is new T_Abstract_Organization_Recurring_Calendar (Instance, -1) with null record;
   type T_Numbered_Organization_Recurring_Calendar (Index : Integer)
      is new T_Abstract_Organization_Recurring_Calendar (null, Index) with null record;

   type T_Abstract_Organizations (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Organizations, Instance, Index) with
   record
      Attributes : SQL_Field_JSON (Ta_Organizations, Instance, N_Attributes, Index);
      Enabled : SQL_Field_Boolean (Ta_Organizations, Instance, N_Enabled, Index);
      Full_Name : SQL_Field_Text (Ta_Organizations, Instance, N_Full_Name, Index);
      ID : SQL_Field_Integer (Ta_Organizations, Instance, N_ID, Index);
      URI : SQL_Field_Text (Ta_Organizations, Instance, N_URI, Index);
   end record;

   type T_Organizations (Instance : Cst_String_Access)
      is new T_Abstract_Organizations (Instance, -1) with null record;
   type T_Numbered_Organizations (Index : Integer)
      is new T_Abstract_Organizations (null, Index) with null record;

   type T_Abstract_Phone_Number_Types (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Phone_Number_Types, Instance, Index) with
   record
      Value : SQL_Field_Text (Ta_Phone_Number_Types, Instance, N_Value, Index);
   end record;

   type T_Phone_Number_Types (Instance : Cst_String_Access)
      is new T_Abstract_Phone_Number_Types (Instance, -1) with null record;
   type T_Numbered_Phone_Number_Types (Index : Integer)
      is new T_Abstract_Phone_Number_Types (null, Index) with null record;

   type T_Abstract_Phone_Numbers (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Phone_Numbers, Instance, Index) with
   record
      ID : SQL_Field_Integer (Ta_Phone_Numbers, Instance, N_ID, Index);
      Kind : SQL_Field_Text (Ta_Phone_Numbers, Instance, N_Kind, Index);
      Value : SQL_Field_Text (Ta_Phone_Numbers, Instance, N_Value, Index);
   end record;

   type T_Phone_Numbers (Instance : Cst_String_Access)
      is new T_Abstract_Phone_Numbers (Instance, -1) with null record;
   type T_Numbered_Phone_Numbers (Index : Integer)
      is new T_Abstract_Phone_Numbers (null, Index) with null record;

   type T_Abstract_Recipient_Visibilities (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Recipient_Visibilities, Instance, Index) with
   record
      Value : SQL_Field_Text (Ta_Recipient_Visibilities, Instance, N_Value, Index);
   end record;

   type T_Recipient_Visibilities (Instance : Cst_String_Access)
      is new T_Abstract_Recipient_Visibilities (Instance, -1) with null record;
   type T_Numbered_Recipient_Visibilities (Index : Integer)
      is new T_Abstract_Recipient_Visibilities (null, Index) with null record;

   type T_Abstract_Recurring_Calendar_Events (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Recurring_Calendar_Events, Instance, Index) with
   record
      Expires : SQL_Field_Time (Ta_Recurring_Calendar_Events, Instance, N_Expires, Index);
      First_Occurrence : SQL_Field_Time (Ta_Recurring_Calendar_Events, Instance, N_First_Occurrence, Index);
      ID : SQL_Field_Integer (Ta_Recurring_Calendar_Events, Instance, N_ID, Index);
      Message : SQL_Field_Text (Ta_Recurring_Calendar_Events, Instance, N_Message, Index);
      Pattern : SQL_Field_JSON (Ta_Recurring_Calendar_Events, Instance, N_Pattern, Index);
      Start : SQL_Field_Integer (Ta_Recurring_Calendar_Events, Instance, N_Start, Index);
      Stop : SQL_Field_Integer (Ta_Recurring_Calendar_Events, Instance, N_Stop, Index);
   end record;

   type T_Recurring_Calendar_Events (Instance : Cst_String_Access)
      is new T_Abstract_Recurring_Calendar_Events (Instance, -1) with null record;
   type T_Numbered_Recurring_Calendar_Events (Index : Integer)
      is new T_Abstract_Recurring_Calendar_Events (null, Index) with null record;

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

   function FK (Self : T_Archive_Message_Queue'Class; Foreign : T_Contacts'Class) return SQL_Criteria;
   function FK (Self : T_Archive_Message_Queue'Class; Foreign : T_Users'Class) return SQL_Criteria;
   function FK (Self : T_Archive_Message_Queue_Recipients'Class; Foreign : T_Organization_Contacts'Class) return SQL_Criteria;
   function FK (Self : T_Archive_Message_Queue_Recipients'Class; Foreign : T_Recipient_Visibilities'Class) return SQL_Criteria;
   function FK (Self : T_Contact_Calendar'Class; Foreign : T_Organization_Contacts'Class) return SQL_Criteria;
   function FK (Self : T_Contact_Calendar'Class; Foreign : T_Calendar_Events'Class) return SQL_Criteria;
   function FK (Self : T_Contact_Phone_Numbers'Class; Foreign : T_Organization_Contacts'Class) return SQL_Criteria;
   function FK (Self : T_Contact_Phone_Numbers'Class; Foreign : T_Phone_Numbers'Class) return SQL_Criteria;
   function FK (Self : T_Contact_Recurring_Calendar'Class; Foreign : T_Organization_Contacts'Class) return SQL_Criteria;
   function FK (Self : T_Contact_Recurring_Calendar'Class; Foreign : T_Recurring_Calendar_Events'Class) return SQL_Criteria;
   function FK (Self : T_Contacts'Class; Foreign : T_Contact_Types'Class) return SQL_Criteria;
   function FK (Self : T_Distribution_Lists'Class; Foreign : T_Organization_Contacts'Class) return SQL_Criteria;
   function FK (Self : T_Distribution_Lists'Class; Foreign : T_Recipient_Visibilities'Class) return SQL_Criteria;
   function FK (Self : T_Message_Queue'Class; Foreign : T_Contacts'Class) return SQL_Criteria;
   function FK (Self : T_Message_Queue'Class; Foreign : T_Users'Class) return SQL_Criteria;
   function FK (Self : T_Message_Queue_Recipients'Class; Foreign : T_Organization_Contacts'Class) return SQL_Criteria;
   function FK (Self : T_Message_Queue_Recipients'Class; Foreign : T_Recipient_Visibilities'Class) return SQL_Criteria;
   function FK (Self : T_Messaging_Addresses'Class; Foreign : T_Messaging_Address_Types'Class) return SQL_Criteria;
   function FK (Self : T_Messaging_End_Points'Class; Foreign : T_Organization_Contacts'Class) return SQL_Criteria;
   function FK (Self : T_Messaging_End_Points'Class; Foreign : T_Messaging_Addresses'Class) return SQL_Criteria;
   function FK (Self : T_Organization_Calendar'Class; Foreign : T_Organizations'Class) return SQL_Criteria;
   function FK (Self : T_Organization_Calendar'Class; Foreign : T_Calendar_Events'Class) return SQL_Criteria;
   function FK (Self : T_Organization_Contacts'Class; Foreign : T_Organizations'Class) return SQL_Criteria;
   function FK (Self : T_Organization_Contacts'Class; Foreign : T_Contacts'Class) return SQL_Criteria;
   function FK (Self : T_Organization_Contacts'Class; Foreign : T_Distribution_Lists'Class) return SQL_Criteria;
   function FK (Self : T_Organization_Recurring_Calendar'Class; Foreign : T_Organizations'Class) return SQL_Criteria;
   function FK (Self : T_Organization_Recurring_Calendar'Class; Foreign : T_Recurring_Calendar_Events'Class) return SQL_Criteria;
   function FK (Self : T_Phone_Numbers'Class; Foreign : T_Phone_Number_Types'Class) return SQL_Criteria;
   function FK (Self : T_Special_Days'Class; Foreign : T_Kinds'Class) return SQL_Criteria;
   function FK (Self : T_User_Ids'Class; Foreign : T_Users'Class) return SQL_Criteria;

   Archive_Message_Queue : T_Archive_Message_Queue (null);
   Archive_Message_Queue_Recipients : T_Archive_Message_Queue_Recipients (null);
   Calendar_Events : T_Calendar_Events (null);
   Contact_Calendar : T_Contact_Calendar (null);
   Contact_Phone_Numbers : T_Contact_Phone_Numbers (null);
   Contact_Recurring_Calendar : T_Contact_Recurring_Calendar (null);
   Contact_Types : T_Contact_Types (null);
   Contacts : T_Contacts (null);
   Dial_Plans : T_Dial_Plans (null);
   Distribution_Lists : T_Distribution_Lists (null);
   Kinds : T_Kinds (null);
   Message_Queue : T_Message_Queue (null);
   Message_Queue_Recipients : T_Message_Queue_Recipients (null);
   Messaging_Address_Types : T_Messaging_Address_Types (null);
   Messaging_Addresses : T_Messaging_Addresses (null);
   Messaging_End_Points : T_Messaging_End_Points (null);
   Organization_Calendar : T_Organization_Calendar (null);
   Organization_Contacts : T_Organization_Contacts (null);
   Organization_Recurring_Calendar : T_Organization_Recurring_Calendar (null);
   Organizations : T_Organizations (null);
   Phone_Number_Types : T_Phone_Number_Types (null);
   Phone_Numbers : T_Phone_Numbers (null);
   Recipient_Visibilities : T_Recipient_Visibilities (null);
   Recurring_Calendar_Events : T_Recurring_Calendar_Events (null);
   Special_Days : T_Special_Days (null);
   User_IDs : T_User_IDs (null);
   Users : T_Users (null);
end Database;
