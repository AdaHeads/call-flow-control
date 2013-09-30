package body Database is
   pragma Style_Checks (Off);
   use type Cst_String_Access;

   function FK (Self : T_Contacts'Class; Foreign : T_Contact_Types'Class) return SQL_Criteria is
   begin
      return Self.Contact_Type = Foreign.Value;
   end FK;

   function FK (Self : T_Distribution_Lists'Class; Foreign : T_Recipient_Visibilities'Class) return SQL_Criteria is
   begin
      return Self.Recipient_Visibility = Foreign.Value;
   end FK;

   function FK (Self : T_End_Points'Class; Foreign : T_Organization_Contacts'Class) return SQL_Criteria is
   begin
      return Self.Contact_Id = Foreign.Contact_Id
         and Self.Organization_Id = Foreign.Organization_Id;
   end FK;

   function FK (Self : T_End_Points'Class; Foreign : T_Address_Types'Class) return SQL_Criteria is
   begin
      return Self.Address_Type = Foreign.Value;
   end FK;

   function FK (Self : T_Organization_Contacts'Class; Foreign : T_Organizations'Class) return SQL_Criteria is
   begin
      return Self.Organization_Id = Foreign.Id;
   end FK;

   function FK (Self : T_Organization_Contacts'Class; Foreign : T_Contacts'Class) return SQL_Criteria is
   begin
      return Self.Contact_Id = Foreign.Id;
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
