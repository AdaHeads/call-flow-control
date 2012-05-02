with GNATCOLL.SQL; use GNATCOLL.SQL;
package Database_Names is
   pragma Style_Checks (Off);
   TC_Contactentity : aliased constant String := "contactentity";
   Ta_Contactentity : constant Cst_String_Access := TC_Contactentity'Access;
   TC_Contactentity_Attributes : aliased constant String := "contactentity_attributes";
   Ta_Contactentity_Attributes : constant Cst_String_Access := TC_Contactentity_Attributes'Access;
   TC_Contactentity_Tags : aliased constant String := "contactentity_tags";
   Ta_Contactentity_Tags : constant Cst_String_Access := TC_Contactentity_Tags'Access;
   TC_Organization : aliased constant String := "organization";
   Ta_Organization : constant Cst_String_Access := TC_Organization'Access;
   TC_Organization_Contactentities : aliased constant String := "organization_contactentities";
   Ta_Organization_Contactentities : constant Cst_String_Access := TC_Organization_Contactentities'Access;

   NC_Ce_Id : aliased constant String := "ce_id";
   N_Ce_Id : constant Cst_String_Access := NC_ce_id'Access;
   NC_Ce_Name : aliased constant String := "ce_name";
   N_Ce_Name : constant Cst_String_Access := NC_ce_name'Access;
   NC_Identifier : aliased constant String := "identifier";
   N_Identifier : constant Cst_String_Access := NC_identifier'Access;
   NC_Json : aliased constant String := "json";
   N_Json : constant Cst_String_Access := NC_json'Access;
   NC_Org_Id : aliased constant String := "org_id";
   N_Org_Id : constant Cst_String_Access := NC_org_id'Access;
   NC_Org_Name : aliased constant String := "org_name";
   N_Org_Name : constant Cst_String_Access := NC_org_name'Access;
end Database_Names;
