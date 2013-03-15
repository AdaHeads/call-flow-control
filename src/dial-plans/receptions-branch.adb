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

with Ada.Exceptions;

package body Receptions.Branch is
   function Action (Item : in     Instance) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Item.Action);
   end Action;

   function Applicable (Item : in     Instance;
                        Call : in     PBX.Call.Identification)
     return Boolean is
   begin
      for Condition of Item.Conditions loop
         if not Condition.True (Call) then
            return False;
         end if;
      end loop;

      return True;
   end Applicable;

   function Create (Conditions : in     Receptions.Conditions.Instance;
                    Action     : in     String) return Instance is
      use Ada.Strings.Unbounded;
   begin
      return (Conditions => Conditions,
              Action     => To_Unbounded_String (Action));
   exception
      when E : Constraint_Error =>
         raise Constraint_Error with "Receptions.Branch.Create: " &
                                     Ada.Exceptions.Exception_Message (E);
   end Create;
end Receptions.Branch;
