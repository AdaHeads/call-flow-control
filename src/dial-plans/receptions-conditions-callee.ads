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

with PBX.Call,
     Receptions.Condition;

private
with Ada.Strings.Unbounded;

package Receptions.Conditions.Callee is
   type Instance is new Receptions.Condition.Instance with private;
   subtype Class is Instance'Class;

   not overriding
   function Create (Number : in String) return Instance;

   overriding
   function True (Item : in Instance;
                  Call : in PBX.Call.Identification) return Boolean;

   overriding
   function Value (Item : in Instance) return String;

   XML_Element_Name : constant String := "callee";
private
   type Instance is new Receptions.Condition.Instance with
      record
         Number : Ada.Strings.Unbounded.Unbounded_String;
      end record;
end Receptions.Conditions.Callee;
