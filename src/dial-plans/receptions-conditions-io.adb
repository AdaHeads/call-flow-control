
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

with Receptions.Condition.IO;

package body Receptions.Conditions.IO is
   function Load (From : in DOM.Core.Node) return Instance is
      use DOM.Core.Nodes, DOM.Support;
      Condition   : DOM.Core.Node;
      Found, Done : Boolean;
   begin
      Check (Element => From,
             Name    => Receptions.Conditions.XML_Element_Name);

      return Result : Instance do
         Condition := First_Child (From);

         loop
            First (Element => Condition,
                   Found   => Found);
            exit when not Found;

            Result.Append (Receptions.Condition.IO.Load (From => Condition));

            Next (Element => Condition,
                  Done    => Done);
            exit when Done;
         end loop;
      end return;
   end Load;
end Receptions.Conditions.IO;
