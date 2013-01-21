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

with System_Messages;

package AMI.Trace is

   type Kind is new System_Messages.Message_Type;

   type Debug_Threshold_Levels is range 0 .. 32;

   No_Debug_Information : constant Debug_Threshold_Levels;

   All_Debug_Information : constant Debug_Threshold_Levels;

   procedure Log (Level : in Kind; Message : in String);

   procedure Debug (Message : in String;
                    Context : in String := "";
                    Level   : in Debug_Threshold_Levels :=
                      Debug_Threshold_Levels'Last);

   procedure Error (Message : in String;
                    Context : in String := "");

   procedure Set_Debug_Threshold (New_Threshold : in Debug_Threshold_Levels);

private

   No_Debug_Information : constant Debug_Threshold_Levels :=
                            Debug_Threshold_Levels'First;

   All_Debug_Information : constant Debug_Threshold_Levels :=
                             Debug_Threshold_Levels'Last;

   Current_Debug_Threshold : Debug_Threshold_Levels := All_Debug_Information;

end AMI.Trace;
