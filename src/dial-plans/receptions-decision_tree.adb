-------------------------------------------------------------------------------
--                                                                           --
--                      Copyright (C) 2012-, AdaHeads K/S                    --
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

with Ada.Exceptions;

package body Receptions.Decision_Tree is
   function Branch (Item : in     Instance;
                    Call : in     PBX.Call.Identification) return String is
   begin
      for Branch of Item.Branches loop
         if Branch.Applicable (Call) then
            return Branch.Action;
         end if;
      end loop;

      return Ada.Strings.Unbounded.To_String (Item.Fall_Back);
   end Branch;

   function Create (Title     : in     String;
                    Branches  : in     Receptions.List_Of_Branches.Vector;
                    Fall_Back : in     String) return Instance is
   begin
      return
        (Title     => Ada.Strings.Unbounded.To_Unbounded_String (Title),
         Branches  => Branches,
         Fall_Back => Ada.Strings.Unbounded.To_Unbounded_String (Fall_Back));
   exception
      when E : Constraint_Error =>
         raise Constraint_Error with "Receptions.Decision_Tree.Create: " &
                                     Ada.Exceptions.Exception_Message (E);
   end Create;

   function Title (Item : in     Instance) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Item.Title);
   end Title;
end Receptions.Decision_Tree;
