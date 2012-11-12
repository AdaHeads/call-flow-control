-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                           Model.Organizations                             --
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

with GNATCOLL.JSON;
with GNATCOLL.SQL.Exec;
with Common;

package Model.Organizations is

   type Organization_Object is tagged private;
   Null_Organization_Object : constant Organization_Object;

   function Full_Name
     (Organization : in Organization_Object)
      return String;

   function Get
     (O_Id : in Organization_Identifier)
      return Organization_Object;
   --  Return the organization that match O_Id, complete with all the contacts.

   procedure Get
     (O_Id    : in Organization_Identifier;
      Process : not null access
        procedure (Element : in Organization_Object'Class));
   --  For every organization with O_Id in the database, an Organization_Object
   --  is handed to Process.

   function Identifier
     (Organization : in Organization_Object)
     return String;

   function JSON
     (Organization : in Organization_Object)
      return GNATCOLL.JSON.JSON_Value;

   function Organization_Id
     (Organization : in Organization_Object)
      return Organization_Identifier;

   function To_JSON
     (Organization : in Organization_Object)
      return Common.JSON_String;
   --  Convert the Contact to a JSON string. This call is convenient wrapper
   --  for the View.Contact.To_JSON function.

private

   type Cursor is new GNATCOLL.SQL.Exec.Forward_Cursor with null record;

   type Organization_Object is tagged
      record
         Full_Name  : Unbounded_String := Null_Unbounded_String;
         Identifier : Unbounded_String := Null_Unbounded_String;
         JSON       : GNATCOLL.JSON.JSON_Value := GNATCOLL.JSON.JSON_Null;
         O_Id       : Organization_Identifier := 0;
      end record;

   Null_Organization_Object : constant Organization_Object
     := (Full_Name  => Null_Unbounded_String,
         Identifier => Null_Unbounded_String,
         JSON       => GNATCOLL.JSON.JSON_Null,
         O_Id       => 0);

   function Organization_Element
     (C : in out Cursor)
      return Organization_Object'Class;
   --  Transforms the low level index based Cursor into the more readable
   --  Organization_Object record.

end Model.Organizations;
