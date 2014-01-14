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

with Ada.Directories;
with Ada.Interrupts.Names;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Util.Process_Identification;

package body Util.Process_Control is

   use Ada.Directories;
   use Util.Process_Identification;

   type Controller_State is (Running, Shutdown, Stopped);

   PID : constant String := Image (Get_Process_ID) & ".pid";
   --  Path to the PID file. If this is empty, then no PID file is written.

   Wait_Called : Boolean := False;
   --  Is set to True when Wait has been called. This is used to test if we've
   --  already called Wait earlier, and if so, ignore the call.

   procedure Create_PID_File;
   procedure Delete_PID_File;
   --  Create and delete the PID file.

   ------------------
   --  Controller  --
   ------------------

   protected Controller is
      entry Check;
      --  If Controller_State is Shutdown the Wait procedure completes. If PID
      --  is non-empty then Delete_PID_File is called.

      procedure Handle_Kill;
      --  Set Controller.State to Shutdown.

      pragma Attach_Handler (Handle_Kill, Ada.Interrupts.Names.SIGINT);
      pragma Attach_Handler (Handle_Kill, Ada.Interrupts.Names.SIGTERM);
      pragma Attach_Handler (Handle_Kill, Ada.Interrupts.Names.SIGPWR);
      --  Handles the SIGINT, SIGTERM and SIGPWR signals. These signalhandlers
      --  change the Controller.State to Shutdown.

      entry Start;
      --  Called by Wait. Set Controller.State to Running and calls
      --  Create_PID_File if PID is non-empty.

   private
      State : Controller_State := Stopped;
   end Controller;

   ----------------------
   -- Create_PID_File  --
   ----------------------

   procedure Create_PID_File
   is
      use Ada.Strings;
      use Ada.Text_IO;

      File  : File_Type;
   begin
      if Exists (PID) then
         raise PID_File_Exists with PID;
      end if;

      Create (File => File,
              Name => PID);
      Put (File => File,
           Item => Fixed.Trim (Image (Get_Process_ID), Both));
      Close (File);
   exception
      when Ada.Text_IO.Name_Error |
           Ada.Text_IO.Use_Error |
           Ada.Text_IO.Device_Error =>
         raise Cannot_Create_PID_File with PID;
   end Create_PID_File;

   -----------------------
   --  Delete_PID_File  --
   -----------------------

   procedure Delete_PID_File
   is
      use Ada.Text_IO;
   begin
      if Exists (PID) then
         Delete_File (PID);
      end if;

   exception
      when Ada.Text_IO.Name_Error |
           Ada.Text_IO.Use_Error |
           Ada.Text_IO.Device_Error =>
         raise Cannot_Delete_PID_File with PID;
   end Delete_PID_File;

   ------------
   --  Stop  --
   ------------

   procedure Stop
   is
   begin
      Controller.Handle_Kill;
   end Stop;

   ------------
   --  Wait  --
   ------------

   procedure Wait
   is
   begin
      if not Wait_Called then
         Wait_Called := True;
         Controller.Start;

         Controller.Check;
         --  We'll hang here until Controller.State is Shutdown.

         Wait_Called := False;
      end if;
   end Wait;

   ------------------
   --  Controller  --
   ------------------

   protected body Controller is
      -------------
      --  Check  --
      -------------

      entry Check when State = Shutdown
      is
      begin
         if PID /= "" then
            Delete_PID_File;
         end if;

         State := Stopped;
      end Check;

      -------------
      --  Start  --
      -------------

      entry Start when State = Stopped
      is
      begin
         State := Running;

         if PID /= "" then
            Create_PID_File;
         end if;
      end Start;

      -------------------
      --  Handle_Kill  --
      -------------------

      procedure Handle_Kill is
      begin
         if State /= Shutdown then
            State := Shutdown;
         end if;
      end Handle_Kill;
   end Controller;

end Util.Process_Control;
