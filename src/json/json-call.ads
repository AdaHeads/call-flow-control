-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                             Call_Queue_JSON                               --
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

with Model.Call;
with Common;

private with GNATCOLL.JSON;

--  This package can return callqueue information and it in JSON format.
package JSON.Call is
   use Common;

   function To_JSON_String (Queue : in Model.Call.Call_List_Type.Map)
                           return JSON_String;
   --  returns the entire Call Queue, in JSON format.

   function To_JSON_String (Length : in Natural)
                            return JSON_String;
   --  returns the number of calls waiting in the calling queue.

   function To_JSON_String (Call : in Model.Call.Call_Type)
                            return JSON_String;
   --  returns the first call in the list.

   function Status_Message (Title   : in String;
                            Message : in String) return JSON_String;
private
   function To_JSON_Object (Call : in Model.Call.Call_Type)
                            return GNATCOLL.JSON.JSON_Value;
   --  takes a call and converts it to a JSON object.
end JSON.Call;
