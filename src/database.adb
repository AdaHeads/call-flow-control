package body Database is
   pragma Style_Checks (Off);
   use type Cst_String_Access;

   function FK (Self : T_Contactentity_Attributes'Class; Foreign : T_Organization'Class) return SQL_Criteria is
   begin
      return Self.Org_Id = Foreign.Org_Id;
   end FK;

   function FK (Self : T_Contactentity_Attributes'Class; Foreign : T_Contactentity'Class) return SQL_Criteria is
   begin
      return Self.Ce_Id = Foreign.Ce_Id;
   end FK;

   function FK (Self : T_Contactentity_Attributes'Class; Foreign : T_Organization_Contactentities'Class) return SQL_Criteria is
   begin
      return Self.Ce_Id = Foreign.Ce_Id
         and Self.Org_Id = Foreign.Org_Id;
   end FK;

   function FK (Self : T_Organization_Contactentities'Class; Foreign : T_Organization'Class) return SQL_Criteria is
   begin
      return Self.Org_Id = Foreign.Org_Id;
   end FK;

   function FK (Self : T_Organization_Contactentities'Class; Foreign : T_Contactentity'Class) return SQL_Criteria is
   begin
      return Self.Ce_Id = Foreign.Ce_Id;
   end FK;
end Database;
