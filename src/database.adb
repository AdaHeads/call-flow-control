package body Database is
   pragma Style_Checks (Off);
   use type Cst_String_Access;

   function FK (Self : T_Contactentity_Attribute'Class; Foreign : T_Contactentity'Class) return SQL_Criteria is
   begin
      return Self.Ce_Id = Foreign.Ce_Id;
   end FK;

   function FK (Self : T_Contactentity_Attribute'Class; Foreign : T_Attribute'Class) return SQL_Criteria is
   begin
      return Self.Attr_Id = Foreign.Attr_Id;
   end FK;

   function FK (Self : T_Contactentity_Organization'Class; Foreign : T_Contactentity'Class) return SQL_Criteria is
   begin
      return Self.Ce_Id = Foreign.Ce_Id;
   end FK;

   function FK (Self : T_Contactentity_Organization'Class; Foreign : T_Organization'Class) return SQL_Criteria is
   begin
      return Self.Org_Id = Foreign.Org_Id;
   end FK;

   function FK (Self : T_Contactentity_Tag'Class; Foreign : T_Contactentity'Class) return SQL_Criteria is
   begin
      return Self.Ce_Id = Foreign.Ce_Id;
   end FK;

   function FK (Self : T_Contactentity_Tag'Class; Foreign : T_Tag'Class) return SQL_Criteria is
   begin
      return Self.Tag_Id = Foreign.Tag_Id;
   end FK;
end Database;
