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

with
  Ada.Command_Line,
  Ada.Text_IO;
with
  SIGHUP.Demonstration_Handlers;

procedure SIGHUP.Test is
begin
   Ada.Text_IO.Put_Line ("Waiting 4 seconds.");
   delay 4.0;
   Ada.Text_IO.Put_Line ("Stopping the SIGHUP handlers.");
   SIGHUP.Stop;

   if SIGHUP.Demonstration_Handlers.Called then
      Ada.Text_IO.Put_Line ("At least one of the demonstration handlers were called.");
   else
      Ada.Text_IO.Put_Line ("None of the demonstration handlers were called.");
   end if;

   if Ada.Command_Line.Argument_Count = 2 or else
        SIGHUP.Demonstration_Handlers.Called then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
   else
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;

   Ada.Text_IO.Put_Line ("Test done.");
end SIGHUP.Test;
