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

package body Dial_Plan_Interface is
   function Callee (PBX  : in Instance;
                    ID   : in Receptions.PBX_Interface.Call'Class)
     return String is
   begin
      raise Program_Error
        with "Dial_Plan_Interface.Callee not implemented yet.";
      return "<callee>";
   end Callee;

   function Caller (PBX  : in Instance;
                    ID   : in Receptions.PBX_Interface.Call'Class)
     return String is
   begin
      raise Program_Error
        with "Dial_Plan_Interface.Caller not implemented yet.";
      return "<caller>";
   end Caller;

   procedure Log (PBX     : in     Instance;
                  Level   : in     Receptions.PBX_Interface.Log_Level;
                  Message : in     String) is
   begin
      raise Program_Error with "Dial_Plan_Interface.Log not implemented yet.";
   end Log;

   function Today_Is (PBX : in Instance;
                      Day : in String) return Boolean is
   begin
      raise Program_Error
        with "Dial_Plan_Interface.Today_Is not implemented yet.";
      return False;
   end Today_Is;
end Dial_Plan_Interface;
