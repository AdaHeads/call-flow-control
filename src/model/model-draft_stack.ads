-------------------------------------------------------------------------------
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

with GNATCOLL.JSON;
with Model.Agent_ID;

package Model.Draft_Stack is
   use GNATCOLL.JSON;

   type Instance is tagged null record;

   subtype Draft_ID is Natural;

   function Stack_Of (Agent : Model.Agent_ID.Agent_ID_Type) return Instance;
   pragma Obsolescent (Stack_Of, "Implement me!");

   function To_JSON (Object : in Instance) return JSON_Value;
   pragma Obsolescent (To_JSON, "Implement me!");

   procedure Delete (Object : in Instance;
                     Id     : in Draft_ID);
   pragma Obsolescent (Delete, "Implement me!");

   function Push (Object : in Instance;
                  Draft  : in JSON_Value) return Draft_ID;
   pragma Obsolescent (Push, "Implement me!");

end Model.Draft_Stack;
