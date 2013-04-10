-------------------------------------------------------------------------------
--                                                                           --
--                     Copyright (C) 2013-, AdaHeads K/S                     --
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

with SQL_Prepared_Statements.Special_Days,
     Storage;

package body Model.Special_Days is

   type None is tagged null record;

   function Forward (C : in out Database_Cursor'Class) return None;
   function Forward (C : in out Database_Cursor'Class) return None is
   begin
      C.Next;
      return (others => <>);
   end Forward;

   procedure Query is
     new Storage.Process_Select_Query (Element           => None,
                                       Database_Cursor   => Database_Cursor,
                                       Cursor_To_Element => Forward);

   function Day_Is (Date : in     Ada.Calendar.Time;
                    Kind : in     String) return Boolean is
      Found : Boolean := False;

      procedure Set (Item : in     None) is
         pragma Unreferenced (Item);
      begin
         Found := True;
      end Set;

      use GNATCOLL.SQL.Exec;
      Aliased_Day_Kind : aliased constant String := Kind;
   begin
      Query (Process_Element    => Set'Access,
             Prepared_Statement => SQL_Prepared_Statements.Special_Days.Query,
             Query_Parameters   => (1 => +Aliased_Day_Kind'Access,
                                    2 => +Date));
      return Found;
   end Day_Is;

   procedure Insert (Date : in     Ada.Calendar.Time;
                     Kind : in     String) is
   begin
      raise Program_Error with "Model.Special_Days.Insert not implemented.";
   end Insert;

   function Today_Is (Kind : in     String) return Boolean is
   begin
      return Day_Is (Date => Ada.Calendar.Clock,
                     Kind => Kind);
   end Today_Is;

end Model.Special_Days;
