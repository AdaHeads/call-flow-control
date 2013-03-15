-------------------------------------------------------------------------------
--                                                                           --
--                      Copyright (C) 2013-, AdaHeads K/S                    --
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

with Ada.Exceptions, Ada.Text_IO; use Ada.Exceptions, Ada.Text_IO;

package body Receptions.Conditions.Inverse is
   use Receptions.Condition_Container;

   not overriding
   function Create (Condition : in Receptions.Condition.Class)
     return Instance is
   begin
      return (Condition => To_Holder (Condition));
   exception
      when E : others =>
         Put_Line (File => Standard_Error,
                   Item => "Receptions.Conditions.Inverse.Create raised " &
                           Exception_Name (E) & " with " &
                           Exception_Message (E) & ".");
         raise;
   end Create;

   overriding
   function True (Item : in Instance;
                  Call : in PBX.Call.Identification) return Boolean is
   begin
      return not Element (Item.Condition).True (Call);
   exception
      when Constraint_Error =>
         raise Constraint_Error
           with "<" & XML_Element_Name & "> object not initialized.";
   end True;

   overriding
   function Value (Item : in Instance) return String is
   begin
      return "not (" & Element (Item.Condition).Value & ")";
   exception
      when Constraint_Error =>
         raise Constraint_Error
           with "<" & XML_Element_Name & "> object not initialized.";
   end Value;
end Receptions.Conditions.Inverse;
