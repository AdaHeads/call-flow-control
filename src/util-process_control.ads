-------------------------------------------------------------------------------
--                                                                           --
--                     Copyright (C) 2014-, AdaHeads K/S                     --
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

--  The Process_Control package enables us to stop the server with the
--  SIGINT, SIGPWR and SIGTERM signals.
--  It is also this package that is responsible for creating the PID file,
--  which by default is always placed in the same directory as the executable.
--  Change the PID constant if you want/need it placed elsewhere.

package Util.Process_Control is

   Package_Name : constant String := "Util.Process_Control";

   pragma Unreserve_All_Interrupts;
   --  Make sure that GNAT does not handle SIGINT interrupts automatically.
   --  Check your compiler for reserved signals.

   Cannot_Create_PID_File : exception;
   --  Is raised if the PID file cannot be created, eg. the server lacks
   --  permissions to write to the current directory.
   Cannot_Delete_PID_File : exception;
   --  Is raised if the PID file cannot be deleted, eg. the server lacks
   --  permissions to write to the current directory or to the PID file itself.
   PID_File_Exists        : exception;
   --  Is raised when the PID file already exists, ie. the server is already
   --  running, or it was shutdown incorrectly.

   procedure Stop;
   --  Shutdown the server.

   procedure Wait;
   --  Wait until either Stop or Controller.Handle_Kill is called. This
   --  procedure is basically what keeps the application running. It's a
   --  replacement for a loop in the main program file.
   --  Wait completes when either Stop is called or the application receives
   --  one of the SIGINT, SIGPWR or SIGTERM signals. After this, wait can be
   --  called again.
   --  Calling Wait multiple times in a row does nothing. Calls proceeding the
   --  first call are ignored.

end Util.Process_Control;
