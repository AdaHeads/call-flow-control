-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                               Event_Parser                                --
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

with Ada.Containers.Ordered_Maps,
     Ada.Strings.Unbounded;

--  This package can parse, the Events that comes from Asterisk.
package Event_Parser is
   use Ada.Strings.Unbounded;

   package Event_List_Type is new Ada.Containers.Ordered_Maps
     (Unbounded_String, Unbounded_String);

   --  Takes a line of text, with key-value pairs structured:
   --  Key: Value<CRLF>
   function Parse (Event_Text : in Unbounded_String)
                   return Event_List_Type.Map;

end Event_Parser;
