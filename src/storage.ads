-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                 Storage                                   --
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

with Common;
with GNATCOLL.SQL.Exec;
with Response;

package Storage is

   ----------------
   --  Generate  --
   ----------------

   generic

      type Cursor is new GNATCOLL.SQL.Exec.Forward_Cursor with private;

      with function Query
        return GNATCOLL.SQL.Exec.Prepared_Statement;
      --  The prepared statement that is used to fetch data from the SQL
      --  database.

      with function To_JSON
        (C : in out Cursor)
         return Common.JSON_String;
      --  Turn the rows in Cursor into a JSON String.

      with function Query_Parameters
        (Response_Object : in Response.Object)
         return GNATCOLL.SQL.Exec.SQL_Parameters;
      --  The parameters needed by the prepared statement given in Query.

   procedure Generate
     (Response_Object : in out Response.Object);
   --  Generates the Value JSON document and sets the corresponding Status
   --  code.

end Storage;
