package body Database is
   pragma Style_Checks (Off);
   use type Cst_String_Access;

   function FK (Self : T_Archive_Message_Queue'Class; Foreign : T_Contacts'Class) return SQL_Criteria is
   begin
      return Self.To_Contact_Id = Foreign.Id;
   end FK;

   function FK (Self : T_Archive_Message_Queue'Class; Foreign : T_Users'Class) return SQL_Criteria is
   begin
      return Self.Taken_By_Agent = Foreign.Name;
   end FK;

   function FK (Self : T_Archive_Message_Queue_Recipients'Class; Foreign : T_Organization_Contacts'Class) return SQL_Criteria is
   begin
      return Self.Contact_Id = Foreign.Contact_Id
         and Self.Organization_Id = Foreign.Organization_Id;
   end FK;

   function FK (Self : T_Archive_Message_Queue_Recipients'Class; Foreign : T_Recipient_Visibilities'Class) return SQL_Criteria is
   begin
      return Self.Recipient_Role = Foreign.Value;
   end FK;

   function FK (Self : T_Contact_Calendar'Class; Foreign : T_Organization_Contacts'Class) return SQL_Criteria is
   begin
      return Self.Contact_Id = Foreign.Contact_Id
         and Self.Organization_Id = Foreign.Organization_Id;
   end FK;

   function FK (Self : T_Contact_Calendar'Class; Foreign : T_Calendar_Events'Class) return SQL_Criteria is
   begin
      return Self.Event_Id = Foreign.Id;
   end FK;

   function FK (Self : T_Contact_Phone_Numbers'Class; Foreign : T_Organization_Contacts'Class) return SQL_Criteria is
   begin
      return Self.Contact_Id = Foreign.Contact_Id
         and Self.Organization_Id = Foreign.Organization_Id;
   end FK;

   function FK (Self : T_Contact_Phone_Numbers'Class; Foreign : T_Phone_Numbers'Class) return SQL_Criteria is
   begin
      return Self.Phone_Number_Id = Foreign.Id;
   end FK;

   function FK (Self : T_Contact_Recurring_Calendar'Class; Foreign : T_Organization_Contacts'Class) return SQL_Criteria is
   begin
      return Self.Contact_Id = Foreign.Contact_Id
         and Self.Organization_Id = Foreign.Organization_Id;
   end FK;

   function FK (Self : T_Contact_Recurring_Calendar'Class; Foreign : T_Recurring_Calendar_Events'Class) return SQL_Criteria is
   begin
      return Self.Event_Id = Foreign.Id;
   end FK;

   function FK (Self : T_Contacts'Class; Foreign : T_Contact_Types'Class) return SQL_Criteria is
   begin
      return Self.Contact_Type = Foreign.Value;
   end FK;

   function FK (Self : T_Distribution_Lists'Class; Foreign : T_Organization_Contacts'Class) return SQL_Criteria is
   begin
      return Self.Send_To_Contact_Id = Foreign.Contact_Id
         and Self.Send_To_Organization_Id = Foreign.Organization_Id;
   end FK;

   function FK (Self : T_Distribution_Lists'Class; Foreign : T_Recipient_Visibilities'Class) return SQL_Criteria is
   begin
      return Self.Recipient_Visibility = Foreign.Value;
   end FK;

   function FK (Self : T_Message_Queue'Class; Foreign : T_Contacts'Class) return SQL_Criteria is
   begin
      return Self.To_Contact_Id = Foreign.Id;
   end FK;

   function FK (Self : T_Message_Queue'Class; Foreign : T_Users'Class) return SQL_Criteria is
   begin
      return Self.Taken_By_Agent = Foreign.Name;
   end FK;

   function FK (Self : T_Message_Queue_Recipients'Class; Foreign : T_Organization_Contacts'Class) return SQL_Criteria is
   begin
      return Self.Contact_Id = Foreign.Contact_Id
         and Self.Organization_Id = Foreign.Organization_Id;
   end FK;

   function FK (Self : T_Message_Queue_Recipients'Class; Foreign : T_Recipient_Visibilities'Class) return SQL_Criteria is
   begin
      return Self.Recipient_Role = Foreign.Value;
   end FK;

   function FK (Self : T_Messaging_Addresses'Class; Foreign : T_Messaging_Address_Types'Class) return SQL_Criteria is
   begin
      return Self.Address_Type = Foreign.Value;
   end FK;

   function FK (Self : T_Messaging_End_Points'Class; Foreign : T_Organization_Contacts'Class) return SQL_Criteria is
   begin
      return Self.Contact_Id = Foreign.Contact_Id
         and Self.Organization_Id = Foreign.Organization_Id;
   end FK;

   function FK (Self : T_Messaging_End_Points'Class; Foreign : T_Messaging_Addresses'Class) return SQL_Criteria is
   begin
      return Self.Address_Id = Foreign.Id;
   end FK;

   function FK (Self : T_Organization_Calendar'Class; Foreign : T_Organizations'Class) return SQL_Criteria is
   begin
      return Self.Organization_Id = Foreign.Id;
   end FK;

   function FK (Self : T_Organization_Calendar'Class; Foreign : T_Calendar_Events'Class) return SQL_Criteria is
   begin
      return Self.Event_Id = Foreign.Id;
   end FK;

   function FK (Self : T_Organization_Contacts'Class; Foreign : T_Organizations'Class) return SQL_Criteria is
   begin
      return Self.Organization_Id = Foreign.Id;
   end FK;

   function FK (Self : T_Organization_Contacts'Class; Foreign : T_Contacts'Class) return SQL_Criteria is
   begin
      return Self.Contact_Id = Foreign.Id;
   end FK;

   function FK (Self : T_Organization_Contacts'Class; Foreign : T_Distribution_Lists'Class) return SQL_Criteria is
   begin
      return Self.Distribution_List_Id = Foreign.Id;
   end FK;

   function FK (Self : T_Organization_Recurring_Calendar'Class; Foreign : T_Organizations'Class) return SQL_Criteria is
   begin
      return Self.Organization_Id = Foreign.Id;
   end FK;

   function FK (Self : T_Organization_Recurring_Calendar'Class; Foreign : T_Recurring_Calendar_Events'Class) return SQL_Criteria is
   begin
      return Self.Event_Id = Foreign.Id;
   end FK;

   function FK (Self : T_Phone_Numbers'Class; Foreign : T_Phone_Number_Types'Class) return SQL_Criteria is
   begin
      return Self.Kind = Foreign.Value;
   end FK;

   function FK (Self : T_Special_Days'Class; Foreign : T_Kinds'Class) return SQL_Criteria is
   begin
      return Self.Kind = Foreign.Id;
   end FK;

   function FK (Self : T_User_Ids'Class; Foreign : T_Users'Class) return SQL_Criteria is
   begin
      return Self.Name = Foreign.Name;
   end FK;
end Database;
