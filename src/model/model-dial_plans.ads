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

with
  Receptions.End_Point,
  Receptions.PBX_Interface;
with
  Model.Dial_Plan_Maps;

package Model.Dial_Plans is
   Not_Found : exception;

   protected Container is
      procedure Clear;
      procedure Reload_All;
      procedure Load (Number : in     String);

      function End_Point (Number : in     String;
                          Call   : in     Receptions.PBX_Interface.Call'Class)
        return Receptions.End_Point.Class;
   private
      Loaded : Model.Dial_Plan_Maps.Map;
   end Container;
end Model.Dial_Plans;