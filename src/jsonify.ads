-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                 JSONIFY                                   --
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

package JSONIFY is

   procedure Contact
     (Cursor : in out GNATCOLL.SQL.Exec.Forward_Cursor;
      JSON   : in out GNATCOLL.JSON.JSON_Value);

   procedure Contact_Attributes
     (Cursor : in out GNATCOLL.SQL.Exec.Forward_Cursor;
      JSON   : in out GNATCOLL.JSON.JSON_Value);

   procedure Contact_Full
     (Cursor : in out GNATCOLL.SQL.Exec.Forward_Cursor;
      JSON   : in out GNATCOLL.JSON.JSON_Value);

   procedure Org_Contacts
     (Cursor : in out GNATCOLL.SQL.Exec.Forward_Cursor;
      JSON   : in out GNATCOLL.JSON.JSON_Value);

   procedure Org_Contacts_Attributes
     (Cursor : in out GNATCOLL.SQL.Exec.Forward_Cursor;
      JSON   : in out GNATCOLL.JSON.JSON_Value);

   procedure Org_Contacts_Full
     (Cursor : in out GNATCOLL.SQL.Exec.Forward_Cursor;
      JSON   : in out GNATCOLL.JSON.JSON_Value);

   procedure Organization
     (Cursor : in out GNATCOLL.SQL.Exec.Forward_Cursor;
      JSON   : in out GNATCOLL.JSON.JSON_Value);

end JSONIFY;
