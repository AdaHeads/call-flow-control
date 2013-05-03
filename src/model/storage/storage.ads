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

with GNATCOLL.SQL.Exec;

package Storage is

   Database_Error : exception;
   --  Is raised if we can't complete a query for one reason or another.

   generic

      type Database_Cursor is new GNATCOLL.SQL.Exec.Forward_Cursor with
        private;
      type Element (<>) is tagged private;

      with function Cursor_To_Element
        (C : in out Database_Cursor'Class)
         return Element;
      --  Takes a Database_Cursor and turns it into an Element.
      --
      --  NOTE: This function has full control over the Database_Cursor,
      --  including moving it forward with C.Next.

   procedure Process_Select_Query
     (Process_Element    : not null access procedure (E : in Element);
      Prepared_Statement : in GNATCOLL.SQL.Exec.Prepared_Statement;
      Query_Parameters   : in GNATCOLL.SQL.Exec.SQL_Parameters :=
                                GNATCOLL.SQL.Exec.No_Parameters);
   --  Hand every Element created by Cursor_To_Element to the Process_Element
   --  procedure.

end Storage;
