-------------------------------------------------------------------------------
--                                                                           --
--                     Copyright (C) 2012-, AdaHeads K/S                     --
--                                                                           --
--  This is free software;  you can redistribute it and/or modify it         --
--  under terms of the  GNU General Public License  as published by the      --
--  Free Software  Foundation;  either version 3,  or (at your  option) any  --
--  later version. This library is distributed in the hope that it will be   --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     --
--  You should have received a copy of the GNU General Public License and    --
--  a copy of the GCC Runtime Library Exception along with this program;     --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
--  <http://www.gnu.org/licenses/>.                                          --
--                                                                           --
-------------------------------------------------------------------------------

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
end Database;
