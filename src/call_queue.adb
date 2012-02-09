-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                               Call_Queue                                  --
--                                                                           --
--                                  BODY                                     --
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

with Ada.Text_IO;
with Task_Controller;

package body Call_Queue is

   task Build_Queue_JSON;

   task body Build_Queue_JSON
   is
      use Task_Controller;

      Interval : constant Duration := 1.0;
   begin
      while Build_Queue_JSON_State = Up loop
         delay Interval;
         Ada.Text_IO.Put_Line ("Building Queue JSON");
      end loop;
   end Build_Queue_JSON;

   protected body Waiting_Calls is
      function Get return List
      is
      begin
         return L;
      end Get;

      procedure Set
        (A_Call : Call)
      is
      begin
         L.Append (A_Call);
      end Set;
   end Waiting_Calls;

end Call_Queue;
