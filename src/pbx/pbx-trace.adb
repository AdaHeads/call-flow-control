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

with Ada.Text_IO; use Ada.Text_IO;

package body PBX.Trace is

   -------------
   --  Debug  --
   -------------

   procedure Debug (Message : in String;
                    Context : in String := "";
                    Level   : in Debug_Threshold_Levels :=
                      Debug_Threshold_Levels'Last) is
   begin
      if Level <= Current_Debug_Threshold and not (Muted (Debug) or
                                                     Muted (Every)) then
         if Context /= "" then
            Put_Line (Package_Name & "." & Kind'Image (Debug) &
                        ": " & Context & ": " & Message);
         else
            Put_Line (Package_Name & "." & Kind'Image (Debug) &
                        ": " & Message);
         end if;
      end if;
   end Debug;

   -------------
   --  Error  --
   -------------

   procedure Error (Message : in String;
                    Context : in String := "") is
   begin
      if not (Muted (Error) or Muted (Every)) then
         if Context /= "" then
            Put_Line (Package_Name & "." & Kind'Image (Error) &
                        ": " & Context & ": " & Message);
         else
            Put_Line (Package_Name & "." & Kind'Image (Error) &
                        ": " & Message);
         end if;
      end if;
   end Error;

   -------------
   --  Fixme  --
   -------------

   procedure Fixme (Message : in String;
                    Context : in String := "") is
   begin
      if not (Muted (Fixme) or Muted (Every)) then
         if Context /= "" then
            Put_Line (Package_Name & "." & Kind'Image (Fixme) &
                        ": " & Context & ": " & Message);
         else
            Put_Line (Package_Name & "." & Kind'Image (Fixme) &
                        ": " & Message);
         end if;
      end if;
   end Fixme;

   -------------------
   --  Information  --
   -------------------

   procedure Information (Message : in String;
                          Context : in String := "") is
   begin
      if not (Muted (Information) or Muted (Every)) then
         if Context /= "" then
            Put_Line
              (Package_Name & "." & Kind'Image (Information) &
                 ": " & Context & ": " & Message);
         else
            Put_Line (Package_Name & "." & Kind'Image (Information) &
                        ": " & Message);
         end if;
      end if;
   end Information;

   ------------
   --  Mute  --
   ------------

   procedure Mute (Trace : in Kind) is
   begin
      Muted (Trace) := True;
   end Mute;

   ---------------------------
   --  Set_Debug_Threshold  --
   ---------------------------

   procedure Set_Debug_Threshold (New_Threshold : in Debug_Threshold_Levels) is
   begin
      Current_Debug_Threshold := New_Threshold;
   end Set_Debug_Threshold;

   --------------
   --  Unmute  --
   --------------

   procedure Unmute (Trace : in Kind) is
   begin
      Muted (Trace) := False;
   end Unmute;

end PBX.Trace;
