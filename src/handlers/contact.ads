-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                 Contact                                   --
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
with AWS.Dispatchers.Callback;
with Common;
with GNATCOLL.SQL.Exec;

package Contact is

   function Read_Callback
     return AWS.Dispatchers.Callback.Handler;
   --  Return a callback handler for the get/contact interface.

private

   use Ada.Strings.Unbounded;

   type Contact_Cursor is new GNATCOLL.SQL.Exec.Forward_Cursor with
     null record;

   type Contact_Row is
      record
         JSON                 : Common.JSON_String;
         Ce_Id                : Natural;
         Ce_Id_Column_Name    : Unbounded_String;
         Ce_Name              : Unbounded_String;
         Ce_Name_Column_Name  : Unbounded_String;
         Is_Human             : Boolean;
         Is_Human_Column_Name : Unbounded_String;
      end record;

   function Element
     (C : in Contact_Cursor)
      return Contact_Row;
   --  Transform the low level index based Contact_Cursor into a more readable
   --  Ada record.

end Contact;
