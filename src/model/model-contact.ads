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

package Model.Contact is

   type Contact_Entity is
      record
         Ce_Id                   : Natural;
         Ce_Id_Column_Name       : Unbounded_String;
         Ce_Name                 : Unbounded_String;
         Ce_Name_Column_Name     : Unbounded_String;
         Is_Human                : Boolean;
         Is_Human_Column_Name    : Unbounded_String;
         Attr_JSON               : JSON_Value;
         Attr_Org_Id             : Natural;
         Attr_Org_Id_Column_Name : Unbounded_String;
      end record;

   function Get
     (Ce_Id : in Natural)
      return Contact_Entity;

end Model.Contact;
