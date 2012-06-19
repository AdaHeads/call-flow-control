-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                 Common                                    --
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

with Ada.Strings.Bounded;
with My_Configuration;

package Common is

   package My renames My_Configuration;

   package JSON_Large is new Ada.Strings.Bounded.Generic_Bounded_Length
     (My.Config.Get (My.JSON_Size_Large));
   --  Used to hold JSON strings which are considered "large", ie. collections
   --  of contacts, attributes or similar.

   package JSON_Small is new Ada.Strings.Bounded.Generic_Bounded_Length
     (My.Config.Get (My.JSON_Size_Small));
   --  Used to hold JSON strings which are considered "small", ie. a single
   --  contact, organization or similar.

end Common;
