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

package PBX.Trace is

   Package_Name : constant String := "PBX.Trace";

   type Debug_Threshold_Levels is range 0 .. 32;

   No_Debug_Information : constant Debug_Threshold_Levels;

   All_Debug_Information : constant Debug_Threshold_Levels;

   type Kind is (Debug, Information, Error, Warning, Critical, Fixme, Every);

   procedure Mute (Trace : in Kind);

   procedure Unmute (Trace : in Kind);

   procedure Debug (Message : in String;
                    Context : in String := "";
                    Level   : in Debug_Threshold_Levels :=
                      Debug_Threshold_Levels'Last);

   procedure Error (Message : in String;
                    Context : in String := "");

   procedure Fixme (Message : in String;
                    Context : in String := "");

   procedure Information (Message : in String;
                          Context : in String := "");
   procedure Set_Debug_Threshold (New_Threshold : in Debug_Threshold_Levels);

private

   Muted : array (Kind) of Boolean := (Debug  => True,
                                       others => False);

   No_Debug_Information : constant Debug_Threshold_Levels :=
                            Debug_Threshold_Levels'First;

   All_Debug_Information : constant Debug_Threshold_Levels :=
                             Debug_Threshold_Levels'Last;

   Current_Debug_Threshold : Debug_Threshold_Levels := All_Debug_Information;

end PBX.Trace;
