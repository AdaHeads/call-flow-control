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

with DOM.Core.Nodes,
     DOM.Support;

with Receptions.Conditions.Calendar_Look_Up,
     Receptions.Conditions.Callee,
     Receptions.Conditions.Clock,
     Receptions.Conditions.Day_Of_Month,
     Receptions.Conditions.Day_Of_Week,
     Receptions.Conditions.Inverse,
     Receptions.Conditions.Month,
     Receptions.Conditions.Week_Number;

package body Receptions.Condition.IO is
   function Load (From : in DOM.Core.Node) return Class is
      package Calendar_Look_Up renames Receptions.Conditions.Calendar_Look_Up;
      package Callee           renames Receptions.Conditions.Callee;
      package Clock            renames Receptions.Conditions.Clock;
      package Day_Of_Month     renames Receptions.Conditions.Day_Of_Month;
      package Day_Of_Week      renames Receptions.Conditions.Day_Of_Week;
      package Inverse          renames Receptions.Conditions.Inverse;
      package Month            renames Receptions.Conditions.Month;
      package Week_Number      renames Receptions.Conditions.Week_Number;

      use DOM.Core.Nodes, DOM.Support;

      Condition : DOM.Core.Node := From;
   begin
      First (Element => Condition);

      if Node_Name (Condition) = Inverse.XML_Element_Name then
         return
           Inverse.Create (Condition => Load (From => First_Child
                                                        (Condition)));
      elsif Node_Name (Condition) = Calendar_Look_Up.XML_Element_Name then
         return
           Calendar_Look_Up.Create (Kind => Attribute (Element => Condition,
                                                       Name    => "kind"));
      elsif Node_Name (Condition) = Callee.XML_Element_Name then
         return
           Callee.Create (Number => Attribute (Element => Condition,
                                               Name    => "number"));

      elsif Node_Name (Condition) = Clock.XML_Element_Name then
         return
           Clock.Create (From => Attribute (Element => Condition,
                                            Name    => "from"),
                         To   => Attribute (Element => Condition,
                                            Name    => "to"));
      elsif Node_Name (Condition) = Day_Of_Month.XML_Element_Name then
         return
           Day_Of_Month.Create (List => Attribute (Element => Condition,
                                                   Name    => "is"));
      elsif Node_Name (Condition) = Day_Of_Week.XML_Element_Name then
         return
           Day_Of_Week.Create (List => Attribute (Element => Condition,
                                                  Name    => "is"));
      elsif Node_Name (Condition) = Month.XML_Element_Name then
         return
           Month.Create (List => Attribute (Element => Condition,
                                            Name    => "is"));
      elsif Node_Name (Condition) = Week_Number.XML_Element_Name then
         return
           Week_Number.Create (List => Attribute (Element => Condition,
                                                  Name    => "is"));
      else
         raise Constraint_Error
           with "<" & Receptions.Conditions.XML_Element_Name &
                "> element contains illegal element <" &
                Node_Name (Condition) & ">.";
      end if;
   end Load;
end Receptions.Condition.IO;
