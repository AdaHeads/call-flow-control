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

with System_Message.Alert,
     System_Message.Critical,
     System_Message.Debug,
     System_Message.Emergency,
     System_Message.Error,
     System_Message.Info,
     System_Message.Notice,
     System_Message.Warning;

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
      pragma Unreferenced (PBX);
      use Receptions.PBX_Interface;
   begin
      case Level is
         when Debug =>
            System_Message.Debug.Dial_Plan     (Message => Message);
         when Information =>
            System_Message.Info.Dial_Plan      (Message => Message);
         when Notice =>
            System_Message.Notice.Dial_Plan    (Message => Message);
         when Warning =>
            System_Message.Warning.Dial_Plan   (Message => Message);
         when Error =>
            System_Message.Error.Dial_Plan     (Message => Message);
         when Critical =>
            System_Message.Critical.Dial_Plan  (Message => Message);
         when Alert =>
            System_Message.Alert.Dial_Plan     (Message => Message);
         when Emergency =>
            System_Message.Emergency.Dial_Plan (Message => Message);
      end case;
   end Log;

   function Today_Is (PBX : in Instance;
                      Day : in String) return Boolean is
   begin
      raise Program_Error
        with "Dial_Plan_Interface.Today_Is not implemented yet.";
      return False;
   end Today_Is;
end Dial_Plan_Interface;
