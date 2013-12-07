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

package body Model.Draft_Stack is
   use GNATCOLL.JSON;

   function Stack_Of (Agent : Model.Agent_ID.Agent_ID_Type) return Instance is
      Obj : Instance;
   begin
      return Obj;
   end Stack_Of;

   function To_JSON (Object : in Instance) return JSON_Value is
   begin
      return GNATCOLL.JSON.Create;
   end To_JSON;

   procedure Delete (Object : in Instance;
                     Id     : in Draft_ID) is
   begin
      raise Program_Error with "Model.Draft_Stack.Delete - not implemented";
   end Delete;

   function Push (Object : in Instance;
                  Draft  : in JSON_Value) return Draft_ID is
   begin
      return 0;
   end Push;

end Model.Draft_Stack;
