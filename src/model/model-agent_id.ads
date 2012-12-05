-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                             Model.Agent_ID                                --
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

package Model.Agent_ID is

   Invalid_ID : exception;

   type Agent_ID_Type is tagged record
      ID : Natural := 0;
   end record;

   Null_Agent_ID : constant Agent_ID_Type;

   function To_String (Agent_ID : in Agent_ID_Type) return String;

   function Create (Agent_ID : in String) return Agent_ID_Type;

   function Validate (Item : in String) return Boolean;

private
   Null_Agent_ID : constant Agent_ID_Type := (ID => Natural'Last);
end Model.Agent_ID;
