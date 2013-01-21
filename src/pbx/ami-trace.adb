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

package body AMI.Trace is

   -------------
   --  Debug  --
   -------------

   procedure Debug (Message : in String;
                    Context : in String := "";
                    Level   : in Debug_Threshold_Levels :=
                      Debug_Threshold_Levels'Last) is
   begin
      if Level <= Current_Debug_Threshold then
         if Context = "" then
            System_Messages.Notify (System_Messages.Debug, Message);
         else
            System_Messages.Notify (System_Messages.Debug,
                                    Context & ": " & Message);

         end if;
      end if;
   end Debug;

   -------------
   --  Error  --
   -------------

   procedure Error (Message : in String;
                    Context : in String := "") is
   begin
      if Context = "" then
         System_Messages.Notify (System_Messages.Error, Message);
      else
         System_Messages.Notify (System_Messages.Error,
                                 Context & ": " & Message);

      end if;
   end Error;

   -----------
   --  Log  --
   -----------

   procedure Log (Level : in Kind; Message : in String) is
   begin
      System_Messages.Notify (System_Messages.Message_Type (Level), Message);
   end Log;
   pragma Obsolescent (Log);

   ---------------------------
   --  Set_Debug_Threshold  --
   ---------------------------

   procedure Set_Debug_Threshold (New_Threshold : in Debug_Threshold_Levels) is
   begin
      Current_Debug_Threshold := New_Threshold;
   end Set_Debug_Threshold;

end AMI.Trace;
