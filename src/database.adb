package body Database is
   pragma Style_Checks (Off);
   use type Cst_String_Access;

   function FK (Self : T_Contactentity_Attributes'Class; Foreign : T_Organization'Class) return SQL_Criteria is
   begin
      return Self.Organization_Id = Foreign.Id;
   end FK;

   function FK (Self : T_Contactentity_Attributes'Class; Foreign : T_Organization_Contactentities'Class) return SQL_Criteria is
   begin
      return Self.Organization_Id = Foreign.Organization_Id
         and Self.Contactentity_Id = Foreign.Contactentity_Id;
   end FK;

   function FK (Self : T_Contactentity_Attributes'Class; Foreign : T_Contactentity'Class) return SQL_Criteria is
   begin
      return Self.Contactentity_Id = Foreign.Id;
   end FK;

   function FK (Self : T_Contactentity_Recipient'Class; Foreign : T_Contactentity'Class) return SQL_Criteria is
   begin
      return Self.Contactentity_Id = Foreign.Id;
   end FK;

   function FK (Self : T_Contactentity_Recipient'Class; Foreign : T_Recipient'Class) return SQL_Criteria is
   begin
      return Self.Recipient_Id = Foreign.Id;
   end FK;

   function FK (Self : T_Organization_Contactentities'Class; Foreign : T_Organization'Class) return SQL_Criteria is
   begin
      return Self.Organization_Id = Foreign.Id;
   end FK;

   function FK (Self : T_Organization_Contactentities'Class; Foreign : T_Contactentity'Class) return SQL_Criteria is
   begin
      return Self.Contactentity_Id = Foreign.Id;
   end FK;

   function FK (Self : T_Recipient'Class; Foreign : T_Recipient_Kind'Class) return SQL_Criteria is
   begin
      return Self.Kind_Id = Foreign.Id;
   end FK;
end Database;
