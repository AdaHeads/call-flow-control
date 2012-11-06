-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                              Model.Contact                                --
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

with Ada.Strings.Unbounded;
with GNATCOLL.JSON;
with GNATCOLL.SQL.Exec;
with Storage;

package Model.Contact is

   use Ada.Strings.Unbounded;

   type Contact_Entity is
      record
         Ce_Id                   : Natural;
         Ce_Id_Column_Name       : Unbounded_String;
         Ce_Name                 : Unbounded_String;
         Ce_Name_Column_Name     : Unbounded_String;
         Is_Human                : Boolean;
         Is_Human_Column_Name    : Unbounded_String;
         Attr_JSON               : GNATCOLL.JSON.JSON_Value;
         Attr_Org_Id             : Natural;
         Attr_Org_Id_Column_Name : Unbounded_String;
      end record;

   procedure For_Each
     (Ce_Id   : in Contactentity_Id;
      Process : not null access
        procedure (Element : in Contact_Entity));
   --  For every contact with Contactentity_Id in the database, a
   --  Contact_Entity element is handed to Process. Note that even though
   --  Contactentity_Id is primary key, more than one element can be returned
   --  due to the fact that a contactentity can exist on more than one context
   --  ie. have more than one set of attributes.

   procedure For_Each
     (Org_Id  : in Organization_Id;
      Process : not null access
        procedure (Element : in Contact_Entity));
   --  TODO: write comment

private

   type Cursor is new GNATCOLL.SQL.Exec.Forward_Cursor with null record;

   function Element
     (C : in Cursor)
      return Contact_Entity;
   --  Transforms the low level index based Cursor into the more readable Row
   --  record.

   procedure Bar is new Storage.Foo (Cursor  => Cursor,
                                     Element => Contact_Entity);

end Model.Contact;
