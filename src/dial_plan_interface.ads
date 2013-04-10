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

with Receptions.PBX_Interface;

package Dial_Plan_Interface is
   type Instance is new Receptions.PBX_Interface.Instance with null record;

   procedure Log (PBX     : in     Instance;
                  Level   : in     Receptions.PBX_Interface.Log_Level;
                  Message : in     String);

   function Caller (PBX  : in Instance;
                    ID   : in Receptions.PBX_Interface.Call'Class)
     return String;
   function Callee (PBX  : in Instance;
                    ID   : in Receptions.PBX_Interface.Call'Class)
     return String;

   function Today_Is (PBX : in Instance;
                      Day : in String) return Boolean;
end Dial_Plan_Interface;
