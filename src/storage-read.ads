-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                               Storage.Read                                --
--                                                                           --
--                                  SPEC                                     --
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

with AWS.Messages;
with Common;

package Storage.Read is

   procedure Contact_Attributes
     (Ce_Id       : in     String;
      Status_Code :    out AWS.Messages.Status_Code;
      Value       :    out Common.JSON_String);
   --  If Ce_Id exists, Value contains the attributes for ONE contact entity
   --  and Status_Code is 200.
   --  If Ce_Id does not exist, Value is an empty JSON string {} and
   --  Status_Code is 404.
   --  If Ce_Id isn't valid, Status_Code is 400.
   --
   --  Note that one contact entity can have several different attribute sets,
   --  depending on the organization the contact belongs to.

   procedure Contact_Full
     (Ce_Id       : in     String;
      Status_Code :    out AWS.Messages.Status_Code;
      Value       :    out Common.JSON_String);
   --  If Ce_Id exists, Value contains the data and attributes for ONE contact
   --   entity and Status_Code is 200.
   --  If Ce_Id does not exist, Value is an empty JSON string {} and
   --  Status_Code is 404.
   --  If Ce_Id isn't valid, Status_Code is 400.

   procedure Org_Contacts
     (Org_Id      : in     String;
      Status_Code :    out AWS.Messages.Status_Code;
      Value       :    out Common.JSON_String);
   --  If Org_Id exists, Value contains ALL contact entities associated with
   --  Org_Id and Status_Code is 200.
   --  If Org_Id does not exist, Value is an empty JSON string {} and
   --  Status_Code is 404.
   --  If Org_Id isn't valid, Status_Code is 400.

   procedure Org_Contacts_Attributes
     (Org_Id      : in     String;
      Status_Code :    out AWS.Messages.Status_Code;
      Value       :    out Common.JSON_String);
   --  If Org_Id exists, Value contains ALL contact entity attributes
   --  associated with Org_Id and Status_Code is 200.
   --  If Org_Id does not exist, Value is an empty JSON string {} and
   --  Status_Code is 404.
   --  If Org_Id isn't valid, Status_Code is 400.

   procedure Org_Contacts_Full
     (Org_Id      : in     String;
      Status_Code :    out AWS.Messages.Status_Code;
      Value       :    out Common.JSON_String);
   --  If Org_Id exists, Value contains ALL contact entity data and attributes
   --  associated with Org_Id and Status_Code is 200.
   --  If Org_Id does not exist, Value is an empty JSON string {} and
   --  Status_Code is 404.
   --  If Org_Id isn't valid, Status_Code is 400.

   procedure Organization
     (Org_Id      : in     String;
      Status_Code :    out AWS.Messages.Status_Code;
      Value       :    out Common.JSON_String);
   --  If Org_Id exists, Value contains the Organization JSON string associated
   --  with Org_Id and Status_Code is 200.
   --  If Org_Id does not exist, Value is an empty JSON string {} and
   --  Status_Code is 404.
   --  If Org_Id isn't valid, Status_Code is 400.

end Storage.Read;
