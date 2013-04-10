package body Database is
   pragma Style_Checks (Off);
   use type Cst_String_Access;

   function FK (Self : T_Contact_Attributes'Class; Foreign : T_Organization'Class) return SQL_Criteria is
   begin
      return Self.Organization_Id = Foreign.Id;
   end FK;

   function FK (Self : T_Contact_Attributes'Class; Foreign : T_Organization_Contacts'Class) return SQL_Criteria is
   begin
      return Self.Organization_Id = Foreign.Organization_Id
         and Self.Contact_Id = Foreign.Contact_Id;
   end FK;

   function FK (Self : T_Contact_Attributes'Class; Foreign : T_Contact'Class) return SQL_Criteria is
   begin
      return Self.Contact_Id = Foreign.Id;
   end FK;

   function FK (Self : T_Contact_Recipients'Class; Foreign : T_Contact'Class) return SQL_Criteria is
   begin
      return Self.Contact_Id = Foreign.Id;
   end FK;

   function FK (Self : T_Contact_Recipients'Class; Foreign : T_Recipient'Class) return SQL_Criteria is
   begin
      return Self.Recipient_Id = Foreign.Id;
   end FK;

   function FK (Self : T_Organization_Contacts'Class; Foreign : T_Organization'Class) return SQL_Criteria is
   begin
      return Self.Organization_Id = Foreign.Id;
   end FK;

   function FK (Self : T_Organization_Contacts'Class; Foreign : T_Contact'Class) return SQL_Criteria is
   begin
      return Self.Contact_Id = Foreign.Id;
   end FK;

   function FK (Self : T_Recipient'Class; Foreign : T_Recipient_Kind'Class) return SQL_Criteria is
   begin
      return Self.Kind_Id = Foreign.Id;
   end FK;

   function FK (Self : T_Special_Days'Class; Foreign : T_Kinds'Class) return SQL_Criteria is
   begin
      return Self.Kind = Foreign.Id;
   end FK;
end Database;
