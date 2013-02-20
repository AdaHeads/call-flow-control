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

with Receptions.Condition;

package Receptions.Conditions.DK.First_Banking_Day_Of_Month is
   type Instance is new Receptions.Condition.Instance with null record;
   subtype Class is Instance'Class;

   not overriding
   function Create return Instance;

   overriding
   function Evaluate (Item : in Instance;
                      Call : in Channel_ID) return Boolean;

   overriding
   function Value (Item : in Instance) return String;

   XML_Element_Name : constant String := "DK-første-bankdag-i-måneden";
end Receptions.Conditions.DK.First_Banking_Day_Of_Month;
